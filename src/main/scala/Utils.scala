package com.elliptic.factoring

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}

def toBits(k: BigInt): Array[Boolean] =
    val bits = ArrayBuffer.empty[Boolean]
    var k_ = k
    while k_ > 0 do
      bits += (k_ & 1) == 1
      k_ = k_ / 2
    bits.toArray

private var inversesCalculated = 0L
def modInverse(a: BigInt, n: BigInt): Either[BigInt, BigInt] =
  val inverse = Try { a.modInverse(n) }
  inversesCalculated += 1
  inverse match
    case Success(s) => Right(s)
    case Failure(e) => Left(a.gcd(n))

def log2(n: Double) = math.log10(n) / math.log10(2.0)

def getInversesCalculated() = inversesCalculated
def resetInversesCalculated() = inversesCalculated = 0