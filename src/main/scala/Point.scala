package com.elliptic.factoring

import scala.annotation.tailrec

type Point = (BigInt, BigInt)

extension (p: Point)
  def +(p2: Point)(using context: CurveContext): Either[BigInt, Point] =
    if p == p2 then p.double
    else
      val lambdaInverse = modInverse(p2._1 - p._1, context.n)
      lambdaInverse.map { inverse =>
        val lambda = ((p2._2 - p._2) * inverse)
        val x = lambda.pow(2) - p._1 - p2._1
        val y = lambda * (p._1 - x) - p._2
        (x.mod(context.n), y.mod(context.n))
      }

  def double(using context: CurveContext): Either[BigInt, Point] =
    val lambdaInverse = modInverse(2 * p._2, context.n)
    lambdaInverse.map { inverse =>
      val lambda = ((3 * p._1.pow(2) + context.b) * inverse)
      val x = (lambda.pow(2) - (2 * p._1).mod(context.n)).mod(context.n)
      val y = (lambda * (p._1 - x)) - p._2
      (x.mod(context.n), y.mod(context.n))
    }

  def **(k: BigInt)(using
      context: CurveContext
  ): Either[(PointSummand, PointSummand), PointSummand] =
    @tailrec
    def iter(
        currentPoint: PointSummand,
        bits: List[Boolean],
        points: List[PointSummand]
    ): Either[(PointSummand, PointSummand), PointSummand] =
      bits match
        case b :: Nil =>
          iter(currentPoint, Nil, if b then currentPoint :: points else points)
        case b :: rest =>
          (currentPoint + currentPoint) match
            case Right(sum) =>
              iter(sum, bits.tail, if b then currentPoint :: points else points)
            case Left(_) => Left(currentPoint -> currentPoint)
        case Nil =>
          points.reverse.tail.foldLeft(
            Right(points.last): Either[
              (PointSummand, PointSummand),
              PointSummand
            ]
          )((pE, p2) =>
            pE.flatMap(p1 =>
              (p1 + p2) match
                case Left(value)  => Left(p1 -> p2)
                case Right(value) => Right(value)
            )
          )
    iter(PointSummand(p, 1), toBits(k).toList, List())

  // Multiplication that keeps track of the multiplicand of the points based on
  // the starting point. e.g. 4p * 7 gives us 28p, we save (28, 28p)
  // three bigints are p1, p2, divisor
  def ***(k: BigInt)(using
      context: CurveContext
  ): Either[(PointSummand, PointSummand, BigInt), PointSummand] =
    @tailrec
    def getPowerOf2Points(
        currentPoint: PointSummand,
        bits: List[Boolean],
        points: List[PointSummand]
    ): Either[(PointSummand, PointSummand, BigInt), List[PointSummand]] =
      bits match
        case b :: rest =>
          (currentPoint + currentPoint) match
            case Right(sum) =>
              getPowerOf2Points(
                sum,
                bits.tail,
                if b then currentPoint :: points else points
              )
            case Left(res) =>
              val result = if b then currentPoint :: points else points
              if result.isEmpty then Left(res) else Right(result)
        case Nil => Right(points)
    val bits = toBits(k).toList
    val pointsE = getPowerOf2Points(PointSummand(p, 1), bits, List())
    pointsE match
      case Left(value) =>
        Left(value)
      case Right(points) =>
        points.tail.foldLeft(
          Right(points.head): Either[
            (PointSummand, PointSummand, BigInt),
            PointSummand
          ]
        )((pE, p2E) =>
          for
            p1 <- pE
            sum <- p1 + p2E
          yield sum
        )

  // Simple multiplication of points
  def *(k: BigInt)(using context: CurveContext): Either[BigInt, Point] =
    @tailrec
    def iter(
        currentPoint: Point,
        bits: List[Boolean],
        points: List[Point]
    ): Either[BigInt, Point] = bits match
      case b :: Nil =>
        iter(currentPoint, Nil, if b then currentPoint :: points else points)
      case true :: rest =>
        (currentPoint + currentPoint) match
          case Right(sum) =>
            iter(sum, bits.tail, currentPoint :: points)
          case divisor => divisor
      case false :: rest =>
        (currentPoint + currentPoint) match
          case Right(sum) =>
            iter(sum, bits.tail, points)
          case divisor => divisor
      case Nil =>
        points.reverse.tail.foldLeft(Right(points.head): Either[BigInt, Point])(
          (pE, p2) => pE.flatMap(_ + p2)
        )
    iter(p, toBits(k).toList, List())

case class PointSummand(p: Point, times: BigInt):
  override def toString(): String = s"P$times:$p"
  def +(pointSummand2: PointSummand)(using
      context: CurveContext
  ): Either[(PointSummand, PointSummand, BigInt), PointSummand] =
    (p + pointSummand2.p) match
      case Left(divisor) =>
        Left((this, pointSummand2, divisor))
      case Right(sum) =>
        Right(PointSummand(sum, times + pointSummand2.times))

  // three bigints are p1, p2, divisor
  // Basic point multiplication
  def **(k: BigInt)(using
      context: CurveContext
  ): Either[(PointSummand, PointSummand, BigInt), PointSummand] =
    @tailrec
    def iter(
        currentPoint: PointSummand,
        bits: List[Boolean],
        points: List[PointSummand]
    ): Either[(PointSummand, PointSummand, BigInt), PointSummand] =
      bits match
        case b :: Nil =>
          iter(currentPoint, Nil, if b then currentPoint :: points else points)
        case b :: rest =>
          (currentPoint + currentPoint) match
            case Right(sum) =>
              iter(
                sum,
                bits.tail,
                if b then currentPoint :: points else points
              )
            case Left(res) => Left(res)
        case Nil =>
          val reversedPoints = points.reverse
          reversedPoints.tail.foldLeft(
            Right(reversedPoints.head): Either[
              (PointSummand, PointSummand, BigInt),
              PointSummand
            ]
          )((pE, p2) => pE.flatMap(p1 => p1 + p2))
    val res = iter(PointSummand(p, times), toBits(k).toList, List())
    res match
      case Left((p1, p2, _)) =>
        if k <= p1.times + p2.times then res
        else this ** (k - p1.times - p2.times)
      case rValue => rValue

  // Multiplication where it successfully multiplies even if a doubling fails e.g.
  // 7P = 1P + 2P + 4P, 4P fails, we set 7P = 1P + 2P = 3P
  // three bigints are p1, p2, divisor
  def ***(k: BigInt)(using
      context: CurveContext
  ): Either[(PointSummand, PointSummand, BigInt), PointSummand] =
    @tailrec
    def getPowerOf2Points(
        currentPoint: PointSummand,
        bits: List[Boolean],
        points: List[Either[(PointSummand, PointSummand, BigInt), PointSummand]]
    ): List[Either[(PointSummand, PointSummand, BigInt), PointSummand]] =
      bits match
        case b :: Nil =>
          getPowerOf2Points(currentPoint, Nil, Right(currentPoint) :: points)
        case b :: rest =>
          (currentPoint + currentPoint) match
            case Right(sum) =>
              getPowerOf2Points(sum, bits.tail, Right(currentPoint) :: points)
            case Left(res) => Left(res) :: points
        case Nil => points
    val bits = toBits(k).toList
    val points = getPowerOf2Points(PointSummand(p, times), bits, List()).reverse
    val relevantPoints = points.zip(bits).filter(_._2).map(_._1)
    val sums = relevantPoints.tail.scanLeft(
      relevantPoints.head: Either[
        (PointSummand, PointSummand, BigInt),
        PointSummand
      ]
    )((pE, p2E) =>
      for
        p1 <- pE
        p2 <- p2E
        sum <- p1 + p2
      yield sum
    )
    val lastSuccessful = sums.collect { case Right(value) => value }.lastOption
    lastSuccessful match
      case None        => sums.last
      case Some(value) => Right(value)
