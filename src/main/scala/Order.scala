package com.elliptic.factoring

import scala.math.BigInt
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import scala.util.control.Breaks._
import javax.swing.text.html.ListView
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.io.File
import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.ops.double

type PrimePower = (BigInt, Int)

extension (number: BigInt)
  def sqrt = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1

    val one = BigInt(1)

    var n = one
    var n1 = next(n, number)

    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }

    while (n1 * n1 > number) {
      n1 -= one
    }

    n1
  }

@main def order(fileName: String) =
  var output: String = ""
  for line <- Source.fromFile(fileName).getLines().drop(1) do
    val values = line.split(',')
    if values(1) == "N/A" then
      output = output ++ s"\nN/A,N/A,N/A,N/A,N/A,N/A,N/A"
    else
      val n = BigInt(values(1))
      val b = BigInt(values(4)).mod(n)
      val c = BigInt(values(5)).mod(n)
      val p: Point = (BigInt(values(6)).mod(n), BigInt(values(7)).mod(n))

      given CurveContext(b, c, n)

      val failedSum = findFailedK(PointSummand(p, 1))

      val t1 = System.nanoTime
      val failedSumFactors = factorsListToPowers(getAllFactorsList(failedSum))
      val orderPowers = findOrderPowers(p, failedSum, failedSumFactors)
      val order = orderPowers.map((prime, power) => prime.pow(power)).product
      val duration = (System.nanoTime - t1) / 1e9d

      val fail = order > (n + 1 + BigInteger(n.toString())
        .sqrt()
        .multiply(BigInteger.TWO))

      println(s"order = $order ${if fail then " ( F A I L ) " else ""}")
      println("")

      if fail == false || true then
        output = output ++ s"\n$order,$n,${if fail then "FAIL" else "SUCCESS"}"
  Files.write(
    Paths.get(File("numbers").toPath().toString() + "/orderData.csv"),
    ("order,p,fail" ++ output)
      .getBytes(StandardCharsets.UTF_8)
  )

@tailrec
def findFailedK(p: PointSummand, k: Int = 2)(using CurveContext): BigInt =
  p *** k match
    case Left((p1, p2, gcd)) =>
      p1.times + p2.times
    case Right(pSum) => findFailedK(pSum, k + 1)

def findOrderPowers(p: Point, sum: BigInt, primePowers: List[PrimePower])(using
    CurveContext
): List[PrimePower] =
  @tailrec def iter(
      factors: List[PrimePower],
      _sum: BigInt,
      successfulFactors: List[List[Either[PrimePower, PrimePower]]]
  ): List[List[Either[PrimePower, PrimePower]]] =
    factors match
      case (prime, power) :: rest =>
        val primePower = prime.pow(power)
        val multiplicand = _sum / primePower
        p *** multiplicand match
          case Left(_) =>
            iter(rest, multiplicand, successfulFactors)
          case Right(_) =>
            val primePowerMultiples =
              calculateAllPrimePowers(p, _sum, (prime, power))
            iter(rest, _sum, primePowerMultiples +: successfulFactors)
      case Nil => successfulFactors
  iter(primePowers, sum, List())
    .map(primePowers =>
      val largestPowerDividingFailedSum =
        primePowers.last.fold(identity, identity)
      val smallestPowerDividingFailedSum = primePowers.collect {
        case Right(value) => value
      }.head
      val power =
        largestPowerDividingFailedSum._2 - smallestPowerDividingFailedSum._2 + 1
      val prime = largestPowerDividingFailedSum._1
      (prime, power)
    )

def calculateAllPrimePowers(p: Point, sum: BigInt, primePower: (BigInt, Int))(
    using CurveContext
): List[Either[PrimePower, PrimePower]] =
  val allPrimePowers = (primePower._2 to 1 by -1).map(primePower._1 -> _).toList
  val rightPowers = allPrimePowers.view
    .map((prime, power) =>
      val multiplicand = sum / prime.pow(power)
      p *** multiplicand match
        case Left(v)  => Left(prime -> power)
        case Right(_) => Right(prime -> power)
    )
    .toList
    .reverse
  rightPowers

def factorsListToPowers(factors: List[BigInt]): List[PrimePower] =
  factors
    .groupBy(identity)
    .map(num => (num._1, num._2.length))
    .toList
    .sortBy(_._1)

def getAllFactorsList(n: BigInt): List[BigInt] =
  given Settings(
    kMax = 5000,
    maxIter = 8096,
    randBitsB = 21,
    randBitsX = 21,
    randBitsY = 21
  )
  @tailrec def iter(
      factors: List[BigInt],
      primeFactors: List[BigInt]
  ): List[BigInt] = factors match
    case num :: rest =>
      if num.isProbablePrime(certainty = 32) then
        iter(rest, num +: primeFactors)
      else if num.mod(2) == 0 || num.mod(3) == 0 then
        trialDivisionFactor(num) match
          case None =>
            iter(rest, num +: primeFactors)
          case Some(p) =>
            iter(List(p, num / p) ++ rest, primeFactors)
      else
        factor(num, 1024).orElse(trialDivisionFactor(num)) match
          case None =>
            iter(rest, num +: primeFactors)
          case Some(p) =>
            iter(List(p, num / p) ++ rest, primeFactors)
    case Nil => primeFactors
  iter(List(n), List())

def trialDivisionFactor(
    n: BigInt,
    attemptedFactor: BigInt = 2
): Option[BigInt] =
  Iterator.iterate(BigInt(2))(_ + 1).takeWhile(_ < n.pow(2)).find(n.mod(_) == 0)
