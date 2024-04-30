package com.elliptic.factoring

import scala.util.Random
import scala.annotation.tailrec
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer
import java.nio.file.Path
import java.io.File
import scala.jdk.StreamConverters._
import scala.compiletime.ops.double
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import scala.annotation.meta.companionObject

given random: Random = Random()

case class Settings(
    kMax: Int,
    maxIter: Int,
    randBitsB: Int,
    randBitsX: Int,
    randBitsY: Int
) {
  override def toString(): String =
    f"k max = ${kMax}, max iter = ${maxIter}, b bits = ${randBitsB}, x bits = ${randBitsX}, y bits = ${randBitsY}"
}

var currentCurve = (BigInt(0), BigInt(0))
var finalK = 0L

val allCurves = ArrayBuffer[(BigInt, BigInt, Boolean, Long)]()

// Import files matching 'x_digit_primes.txt', split them up into pairs of numbers, e.g.
// [101, 3], [5, 5], [13, 23], . . .
// Multiply the numbers into pairs
// 303, 25, 299, . . .
// Use the algorithm to factor them back into primes, then save the collected data into a csv file  
@main def main(): Unit =
  val primeFiles = File("./numbers")
    .listFiles()
    .filter(_.getName().endsWith("_digit_primes.txt"))
    .toList
    .sortBy(_.getName().takeWhile(_ != '_').toInt)
  val largePrimeStrings = List("themselves")
  for file <- primeFiles do
    val digits = file.getName().takeWhile(_ != '_').toInt
    val primes = Source.fromFile(file).getLines().map(BigInt(_)).toList
    val selectedLargePrime = largePrimeStrings.concat("")
    val composites = primes.grouped(2).map(_.product).toList
    val bits = 8
    val k_maxs = List(108, 116, 118) // List of k_max values to run primes through
    val outputFileName =
      s"data_${digits}_digit_primes_mult_by_${selectedLargePrime}.csv"
    val output = for k_max <- k_maxs yield
      println(
        s"\nFactoring ${digits}-digit primes multiplied by ${selectedLargePrime}, k_max = $k_max"
      )
      val outputs = for composite <- composites yield
        resetInversesCalculated()
        resetCurvesGenerated()
        val result = tryFactor(composite, k_max, Some(bits))
        // val result = tryFactor(composite, k_max, None)
        print(f".")
        ResultOutput(
          composite,
          result,
          getInversesCalculated(),
          getCurvesGenerated(),
          result.map(_ => currentCurve),
          result.map(_ => currentPoint),
          result.map(_ => currentK),
          k_max,
          bits,
          result.map(_ => currentFailedXY)
        )
      outputs.map(_.toString()).mkString("\n")
    // println(s"Saving $output to file")
    Files.write(
      Paths.get("./numbers/" + outputFileName),
      ("n,factor,inverses calculated,curves generated,curve b,curve c,px,py,k,k_max,b_bits,x,y\n" ++ output
        .mkString("\n"))
        .getBytes(StandardCharsets.UTF_8)
    )

var currentPoint: (BigInt, BigInt) = (0, 0)
var currentFailedXY: (BigInt, BigInt) = (0, 0)
def tryFactor(
    n: BigInt,
    k_max: Int,
    bits: Option[Int] = None,
    iter: Int = 0
): Option[BigInt] =
  if iter > 1024 then None
  else
    val curveContext = bits match
      case None        => newCurveContextBounded(n)
      case Some(value) => newCurveContextBounded2(n, value)
    given CurveContext = curveContext
    currentCurve = (curveContext.b, curveContext.c) // OUTPUTTING VARIABLE
    currentPoint = curveContext.p // OUTPUTTING VARIABLE
    tryFactorialMultiply(PointSummand(curveContext.p, 1), 2, k_max) match
      case Some((x, y, gcd)) if gcd != n =>
        currentFailedXY = (x, y)
        allCurves.append((currentCurve._1, currentCurve._2, true, currentK))
        Some(gcd)
      case _ =>
        allCurves.append((currentCurve._1, currentCurve._2, false, currentK))
        tryFactor(n, k_max, bits, iter + 1)

case class ResultOutput(
    n: BigInt,
    factor: Option[BigInt],
    inversesCalculated: Long,
    curvesGenerated: Long,
    curveSucceeded: Option[(BigInt, BigInt)],
    pointSucceeded: Option[(BigInt, BigInt)],
    kSucceeded: Option[Long],
    k_max: Long,
    b_bits: Int,
    sumFailed: Option[(BigInt, BigInt)]
) {
  override def toString(): String =
    val factorStr = factor.map(_.toString).getOrElse("N/A")
    val curveStr = curveSucceeded
      .map(curve => f"${curve._1},${curve._2}")
      .getOrElse("N/A,N/A")
    val pointStr = pointSucceeded
      .map(point => f"${point._1},${point._2}")
      .getOrElse("N/A,N/A")
    val kString = kSucceeded.map(_.toString()).getOrElse("N/A")
    val sumFailedString =
      sumFailed.map(s => s"${s._1},${s._2}").getOrElse("N/A")
    f"$n,$factorStr,$inversesCalculated,$curvesGenerated,$curveStr,$pointStr,$kString,$k_max,$b_bits,$sumFailedString"
}

@tailrec
def factor(n: BigInt, k_max: Long, iter: Int = 1)(using
    settings: Settings
): Option[BigInt] =
  if iter > settings.maxIter then None
  else
    var p: Point = (
      BigInt(settings.randBitsX, random),
      BigInt(settings.randBitsY, random)
    )
    newCurveContext(p, n, settings.randBitsB) match
      case None =>
        factor(n, k_max, iter + 1)
      case Some(c) =>
        given CurveContext = c
        currentCurve = (c.b, c.c) // OUTPUTTING VARIABLE
        currentPoint = p // OUTPUTTING VARIABLE
        tryFactorialMultiply(PointSummand(p, 1), 2, k_max) match
          case Some((x, y, gcd)) if gcd != n =>
            currentFailedXY = (x, y)
            allCurves.append((currentCurve._1, currentCurve._2, true, currentK))
            Some(gcd)
          case _ =>
            allCurves.append(
              (currentCurve._1, currentCurve._2, false, currentK)
            )
            factor(n, k_max, iter + 1)

var currentK: Long = 0L
@tailrec
def tryFactorialMultiply(p: PointSummand, k: Long, k_max: Long)(using
    CurveContext
): Option[(BigInt, BigInt, BigInt)] =
  if k > k_max then
    currentK = k_max
    None
  else
    currentK = k
    p ** k match
      case Right(newP) =>
        tryFactorialMultiply(newP, k + 1, k_max)
      case Left((p1, p2, divisor)) =>
        finalK = k
        Some((p1.times, p2.times, divisor))
