package dotty.tools
package dotc

import core.Contexts._
import reporting.Reporter
import io.AbstractFile

import scala.annotation.internal.sharable

/** A main class for running compiler benchmarks. Can instantiate a given
 *  number of compilers and run each (sequentially) a given number of times
 *  on the same sources.
 */
object Bench extends Driver:

  @sharable private var numRuns = 1
  @sharable private var numCompilers = 1
  @sharable private var curCompiler = 0

  private def ntimes(n: Int)(op: => Reporter): Reporter =
    (0 until n).foldLeft(emptyReporter)((_, _) => op)

  @sharable private var times: Array[Int] = _

  override def doCompile(compiler: Compiler, files: List[AbstractFile])(using Context): Reporter =
    var reporter: Reporter = emptyReporter
    for i <- 0 until numRuns do
      val start = System.nanoTime()
      reporter = super.doCompile(compiler, files)
      times(curCompiler + i) = ((System.nanoTime - start) / 1000000).toInt
      println(s"time elapsed: ${times(curCompiler + i)}ms")
      if ctx.settings.Xprompt.value then
        print("hit <return> to continue >")
        System.in.nn.read()
        println()
    reporter

  def extractNumArg(args: Array[String], name: String, default: Int = 1): (Int, Array[String]) = {
    val pos = args indexOf name
    if (pos < 0) (default, args)
    else (args(pos + 1).toInt, (args take pos) ++ (args drop (pos + 2)))
  }

  def reportTimes() =
    val best = times.sorted
    val measured = numCompilers * numRuns / 3
    val avgBest = best.take(measured).sum / measured
    val avgLast = times.reverse.take(measured).sum / measured
    println(s"best out of ${numCompilers * numRuns} runs: ${best(0)}")
    println(s"average out of best $measured: $avgBest")
    println(s"average out of last $measured: $avgLast")

  override def process(args: Array[String]): Reporter =
    val (numCompilers, args1) = extractNumArg(args, "#compilers")
    val (numRuns, args2) = extractNumArg(args1, "#runs")
    val (waitBefore, args3) = extractNumArg(args2, "#wait-after", -1)
    this.numRuns = numRuns
    this.numCompilers = numCompilers
    this.curCompiler = 0
    this.times = new Array[Int](numCompilers * numRuns)
    var reporter: Reporter = emptyReporter
    for i <- 0 until numCompilers do
      if waitBefore == curCompiler then
        print("hit <return> to continue >")
        System.in.nn.read()
      reporter = super.process(args3)
      curCompiler += 1
    reportTimes()
    reporter

end Bench


