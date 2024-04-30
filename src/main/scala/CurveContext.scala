package com.elliptic.factoring

import scala.util.Random
import scala.annotation.tailrec

// Curve context consists of a curve, the number to be factored, and the starting point if needed
case class CurveContext(b: BigInt, c: BigInt, n: BigInt, p: Point = (0, 0))

private var curvesGenerated = 0L

// Generate new curve context using specified number of bits to generate b
@tailrec
def newCurveContext(p: Point, n: BigInt, b_bits: Int, iter: Int = 1)(using
    random: Random
): Option[CurveContext] =
  if iter > 16 then None
  else
    // range 0 to (2 ^ numBits - 1)
    val b = BigInt(b_bits, random) + 1
    val c = p._2.pow(2) - p._1.pow(3) - b * p._1
    // gcd(4a^3 - 27b^2, p) = 1 means we have singular curve re-generate
    if (4 * b.pow(3) - 27 * c.pow(2)).gcd(n) == 1 then
      curvesGenerated += 1 // OUTPUTTING VARIABLE
      Some(CurveContext(b, c, n))
    else newCurveContext(p, n, b_bits, iter + 1)

// Generate new curve context using size of n to find good number of bits for generating b,
// and for b, c, px, and py < n
@tailrec
def newCurveContextBounded(n: BigInt, iter: Int = 1)(using
    random: Random
): CurveContext =
  if iter > 256 then throw Exception(s"COULDN'T GENERATE CURVE! ($n)")
  else
    val bits = (n.toString().length() / math.log10(2) / 2).ceil.toInt
    // val bits = 3
    var p: Point = (
      BigInt(bits - 1, random),
      BigInt(bits, random)
    )
    val b = BigInt(bits, random) + 1
    val c = p._2.pow(2) - p._1.pow(3) - b * p._1
    // gcd(4a^3 - 27b^2, p) = 1 means we have singular curve re-generate
    if ((4 * b.pow(3) - 27 * c.pow(2)).gcd(n) == 1) && List(b, c, p._1, p._2)
        .forall(_ < n)
    then
      curvesGenerated += 1 // OUTPUTTING VARIABLE
      println(s"Generating curve y^2 = x^3 + ${b}x + $c, P = $p")
      CurveContext(b, c, n, p)
    else newCurveContextBounded(n, iter + 1)

// Generate new curve context using specified number of bits for generating b
@tailrec
def newCurveContextBounded2(n: BigInt, bits: Int, iter: Int = 1)(using
    random: Random
): CurveContext =
  if iter > 256 then throw Exception(s"COULDN'T GENERATE CURVE! ($n)")
  else
    var p: Point = (
      BigInt(bits - 1, random),
      BigInt(bits, random)
    )
    val b = BigInt(bits, random) + 1
    val c = p._2.pow(2) - p._1.pow(3) - b * p._1
    // gcd(4a^3 - 27b^2, p) = 1 means we have singular curve re-generate
    if ((4 * b.pow(3) - 27 * c.pow(2)).gcd(n) == 1) && List(b, c, p._1, p._2)
        .forall(_ < n)
    then
      curvesGenerated += 1 // OUTPUTTING VARIABLE
      CurveContext(b, c, n, p)
    else newCurveContextBounded2(n, iter + 1)

def getCurvesGenerated() = curvesGenerated
def resetCurvesGenerated() = curvesGenerated = 0
