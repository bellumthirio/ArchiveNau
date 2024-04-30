file://<HOME>/Downloads/Archive%20(2)/src/main/scala/Main.scala
### java.lang.AssertionError: assertion failed: tree: Ensuring[A](null: (x$4 : com.elliptic.factoring.CurveContext)), pt: <notype>

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
uri: file://<HOME>/Downloads/Archive%20(2)/src/main/scala/Main.scala
text:
```scala
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

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:3598)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3590)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Implicits.tryConversion$1(Implicits.scala:1137)
	dotty.tools.dotc.typer.Implicits.typedImplicit(Implicits.scala:1168)
	dotty.tools.dotc.typer.Implicits.typedImplicit$(Implicits.scala:819)
	dotty.tools.dotc.typer.Typer.typedImplicit(Typer.scala:117)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.tryImplicit(Implicits.scala:1243)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.rank$1(Implicits.scala:1342)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.searchImplicit(Implicits.scala:1512)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.searchImplicit(Implicits.scala:1540)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.bestImplicit(Implicits.scala:1573)
	dotty.tools.dotc.typer.Implicits.inferImplicit(Implicits.scala:1061)
	dotty.tools.dotc.typer.Implicits.inferImplicit$(Implicits.scala:819)
	dotty.tools.dotc.typer.Typer.inferImplicit(Typer.scala:117)
	dotty.tools.dotc.typer.Implicits.inferView(Implicits.scala:857)
	dotty.tools.dotc.typer.Implicits.inferView$(Implicits.scala:819)
	dotty.tools.dotc.typer.Typer.inferView(Typer.scala:117)
	dotty.tools.dotc.typer.Implicits.viewExists(Implicits.scala:832)
	dotty.tools.dotc.typer.Implicits.viewExists$(Implicits.scala:819)
	dotty.tools.dotc.typer.Typer.viewExists(Typer.scala:117)
	dotty.tools.dotc.typer.Implicits.ignoredConvertibleImplicits$1$$anonfun$3(Implicits.scala:961)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:479)
	scala.collection.Iterator.isEmpty(Iterator.scala:466)
	scala.collection.Iterator.isEmpty$(Iterator.scala:466)
	scala.collection.AbstractIterator.isEmpty(Iterator.scala:1300)
	scala.collection.View$Filter.isEmpty(View.scala:146)
	scala.collection.IterableOnceOps.nonEmpty(IterableOnce.scala:853)
	scala.collection.IterableOnceOps.nonEmpty$(IterableOnce.scala:853)
	scala.collection.AbstractIterable.nonEmpty(Iterable.scala:933)
	dotty.tools.dotc.reporting.MissingImplicitArgument.noChainConversionsNote$1(messages.scala:2929)
	dotty.tools.dotc.reporting.MissingImplicitArgument.msgPostscript$$anonfun$4(messages.scala:2944)
	scala.Option.orElse(Option.scala:477)
	dotty.tools.dotc.reporting.MissingImplicitArgument.msgPostscript(messages.scala:2944)
	dotty.tools.dotc.reporting.Message.message$$anonfun$1(Message.scala:344)
	dotty.tools.dotc.reporting.Message.inMessageContext(Message.scala:340)
	dotty.tools.dotc.reporting.Message.message(Message.scala:344)
	dotty.tools.dotc.reporting.Message.isNonSensical(Message.scala:321)
	dotty.tools.dotc.reporting.HideNonSensicalMessages.isHidden(HideNonSensicalMessages.scala:16)
	dotty.tools.dotc.reporting.HideNonSensicalMessages.isHidden$(HideNonSensicalMessages.scala:10)
	dotty.tools.dotc.interactive.InteractiveDriver$$anon$5.isHidden(InteractiveDriver.scala:156)
	dotty.tools.dotc.reporting.Reporter.issueUnconfigured(Reporter.scala:156)
	dotty.tools.dotc.reporting.Reporter.go$1(Reporter.scala:181)
	dotty.tools.dotc.reporting.Reporter.issueIfNotSuppressed(Reporter.scala:200)
	dotty.tools.dotc.reporting.Reporter.report(Reporter.scala:203)
	dotty.tools.dotc.reporting.StoreReporter.report(StoreReporter.scala:50)
	dotty.tools.dotc.report$.error(report.scala:68)
	dotty.tools.dotc.typer.Typer.issueErrors$1$$anonfun$1(Typer.scala:3811)
	scala.runtime.function.JProcedure3.apply(JProcedure3.java:15)
	scala.runtime.function.JProcedure3.apply(JProcedure3.java:10)
	scala.collection.LazyZip3.foreach(LazyZipOps.scala:248)
	dotty.tools.dotc.typer.Typer.issueErrors$1(Typer.scala:3813)
	dotty.tools.dotc.typer.Typer.addImplicitArgs$1(Typer.scala:3835)
	dotty.tools.dotc.typer.Typer.adaptNoArgsImplicitMethod$1(Typer.scala:3852)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4047)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4277)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3590)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3303)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1704)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3064)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3115)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3303)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1168)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3058)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3115)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.$anonfun$14(Typer.scala:1263)
	dotty.tools.dotc.typer.Applications.harmonic(Applications.scala:2338)
	dotty.tools.dotc.typer.Applications.harmonic$(Applications.scala:352)
	dotty.tools.dotc.typer.Typer.harmonic(Typer.scala:117)
	dotty.tools.dotc.typer.Typer.typedIf(Typer.scala:1265)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3059)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3115)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3303)
	dotty.tools.dotc.typer.Typer.$anonfun$57(Typer.scala:2486)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:243)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2486)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3026)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3114)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3213)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3259)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2669)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3038)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3042)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3114)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3213)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3259)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2812)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3083)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3115)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3303)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:44)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$adapted$1(TyperPhase.scala:50)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:440)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:50)
	dotty.tools.dotc.typer.TyperPhase.runOn$$anonfun$3(TyperPhase.scala:84)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:84)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:246)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:262)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:270)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:279)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:71)
	dotty.tools.dotc.Run.compileUnits(Run.scala:279)
	dotty.tools.dotc.Run.compileSources(Run.scala:194)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:34)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:217)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: tree: Ensuring[A](null: (x$4 : com.elliptic.factoring.CurveContext)), pt: <notype>