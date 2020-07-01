package dotty.tools.dotc

import java.nio.file.{Files, Paths}

import dotty.tools.FatalError
import config.{CompilerCommand, Settings}
import core.Comments.{ContextDoc, ContextDocstrings}
import core.Contexts.{Context, ContextBase, inContext, ctx}
import core.{MacroClassLoader, Mode, TypeError}
import core.StdNames.nme
import dotty.tools.dotc.ast.Positioned
import dotty.tools.io.File
import reporting._
import core.Decorators._
import config.Feature

import scala.util.control.NonFatal
import fromtasty.{TASTYCompiler, TastyFileUtil}

/** Run the Dotty compiler.
 *
 *  Extending this class lets you customize many aspect of the compilation
 *  process, but in most cases you only need to call [[process]] on the
 *  existing object [[Main]].
 */
class Driver {

  protected def newCompiler(implicit ctx: Context): Compiler =
    if (ctx.settings.fromTasty.value) new TASTYCompiler
    else new Compiler

  protected def emptyReporter: Reporter = new StoreReporter(null)

  protected def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter =
    doCompile(compiler, fileNames, _.compile(fileNames))

  protected def doCompileVirtual(compiler: Compiler, virtualFiles: List[interfaces.incremental.SourceHandle])(implicit ctx: Context): Reporter =
    doCompile(compiler, virtualFiles.map(_.id), _.compileVirtual(virtualFiles))

  private def doCompile(compiler: Compiler, fileNames: List[String], compileOp: Run => Unit)(implicit ctx: Context): Reporter =
    if (fileNames.nonEmpty)
      try
        val run = compiler.newRun
        compileOp(run)

        def finish(run: Run): Unit =
          run.printSummary()
          if !ctx.reporter.errorsReported && run.suspendedUnits.nonEmpty then
            val suspendedUnits = run.suspendedUnits.toList
            if (ctx.settings.XprintSuspension.value)
              ctx.echo(i"compiling suspended $suspendedUnits%, %")
            val run1 = compiler.newRun
            for unit <- suspendedUnits do unit.suspended = false
            run1.compileUnits(suspendedUnits)
            finish(run1)

        finish(run)
      catch
        case ex: FatalError  =>
          ctx.error(ex.getMessage) // signals that we should fail compilation.
        case ex: TypeError =>
          println(s"${ex.toMessage} while compiling ${fileNames.mkString(", ")}")
          throw ex
        case ex: Throwable =>
          println(s"$ex while compiling ${fileNames.mkString(", ")}")
          throw ex
    ctx.reporter
  end doCompile

  protected def initCtx: Context = (new ContextBase).initialCtx

  protected def sourcesRequired: Boolean = true

  def initialSetup(args: Array[String], rootCtx: Context): (Settings.ArgsSummary, Context) = {
    val ictx = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ictx)
    ictx.setSettings(summary.sstate)
    MacroClassLoader.init(ictx)
    Positioned.updateDebugPos(ictx)

    inContext(ictx) {
      if !ctx.settings.YdropComments.value || ctx.mode.is(Mode.ReadComments) then
        ictx.setProperty(ContextDoc, new ContextDocstrings)
      if Feature.enabledBySetting(nme.Scala2Compat) && false then // TODO: enable
        ctx.warning("-language:Scala2Compat will go away; use -source 3.0-migration instead")
      (summary, ctx)
    }
  }

  final def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val (summary, ictx) = initialSetup(args, rootCtx)
    inContext(ictx) {
      val fileNames = CompilerCommand.checkUsage(summary, summary => sourcesRequired && summary.arguments.isEmpty)
      fromTastySetup(fileNames, ctx)
    }
  }

  type VirtualSources = Either[List[String], List[interfaces.incremental.SourceHandle]]

  final def setupVirtual(args: Array[String], srcs: Array[interfaces.incremental.SourceHandle], rootCtx: Context): (VirtualSources, Context) = {
    val (summary, ictx) = initialSetup(args, rootCtx)
    inContext(ictx) {
      val fileNames = CompilerCommand.checkUsage(summary, _ => sourcesRequired && srcs.isEmpty)
      if fileNames.nonEmpty then
        ctx.error("mixing of path-based and virtual sources")
        (Left(Nil), ctx)
      else
        fromTastyVirtualSetup(srcs, ctx)
    }
  }

  /** Setup extra classpath and figure out class names for tasty file inputs */
  protected def extractClassesFromTasty(fileNames0: List[String], ctx0: Context): (List[String], Context) =
    // Resolve classpath and class names of tasty files
    val (classPaths, classNames) = fileNames0.flatMap { name =>
      val path = Paths.get(name)
      if (name.endsWith(".jar"))
        new dotty.tools.io.Jar(File(name)).toList.collect {
          case e if e.getName.endsWith(".tasty") =>
            (name, e.getName.stripSuffix(".tasty").replace("/", "."))
        }
      else if (!name.endsWith(".tasty"))
        ("", name) :: Nil
      else if (Files.exists(path))
        TastyFileUtil.getClassName(path) match {
          case Some(res) => res:: Nil
          case _ =>
            ctx0.error(s"Could not load classname from $name.")
            ("", name) :: Nil
        }
      else {
        ctx0.error(s"File $name does not exist.")
        ("", name) :: Nil
      }
    }.unzip
    val ctx1 = ctx0.fresh
    val classPaths1 = classPaths.distinct.filter(_ != "")
    val fullClassPath = (classPaths1 :+ ctx1.settings.classpath.value(ctx1)).mkString(java.io.File.pathSeparator)
    ctx1.setSetting(ctx1.settings.classpath, fullClassPath)
    (classNames, ctx1)
  end extractClassesFromTasty

  /** Setup extra classpath and figure out class names for tasty file inputs */
  protected def fromTastyVirtualSetup(fileNames0: Array[interfaces.incremental.SourceHandle], ctx0: Context): (VirtualSources, Context) =
    if ctx0.settings.fromTasty.value(ctx0) then
      inContext(ctx0) {
        val (virtual, real) = fileNames0.partitionMap { src =>
          val p = src.pathOrNull
          if p == null then Left(src)
          else Right(p.toString)
        }
        if virtual.nonEmpty then
          // TODO: if we adapt extractClassesFromTasty to work with virtual files then we need to be able to
          // request a new virtual file with a different path so that it can be used as a classpath source.
          ctx.error(s"The ${ctx.settings.fromTasty.name} compiler flag can only be used with real files, the following sources are virtual:\n${virtual.mkString(", ")}")
          (Left(Nil), ctx)
        else
          val (classes, ctx1) = extractClassesFromTasty(real.toList, ctx)
          (Left(classes), ctx1)
      }
    else
      (Right(fileNames0.toList), ctx0)

  /** Setup extra classpath and figure out class names for tasty file inputs */
  protected def fromTastySetup(fileNames0: List[String], ctx0: Context): (List[String], Context) =
    if (ctx0.settings.fromTasty.value(ctx0)) extractClassesFromTasty(fileNames0, ctx0)
    else (fileNames0, ctx0)

  /** Entry point to the compiler that can be conveniently used with Java reflection.
   *
   *  This entry point can easily be used without depending on the `dotty` package,
   *  you only need to depend on `dotty-interfaces` and call this method using
   *  reflection. This allows you to write code that will work against multiple
   *  versions of dotty without recompilation.
   *
   *  The trade-off is that you can only pass a SimpleReporter to this method
   *  and not a normal Reporter which is more powerful.
   *
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/compiler/test/dotty/tools/dotc/InterfaceEntryPointTest.scala]]
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param simple     Used to log errors, warnings, and info messages.
   *                    The default reporter is used if this is `null`.
   *  @param callback   Used to execute custom code during the compilation
   *                    process. No callbacks will be executed if this is `null`.
   *  @return
   */
  final def process(args: Array[String], simple: interfaces.SimpleReporter,
    callback: interfaces.CompilerCallback): interfaces.ReporterResult = {
    val reporter = if (simple == null) null else Reporter.fromSimpleReporter(simple)
    process(args, reporter, callback)
  }

  /** Principal entry point to the compiler.
   *
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/compiler/test/dotty/tools/dotc/EntryPointsTest.scala.disabled]]
   *  in method `runCompiler`
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param reporter   Used to log errors, warnings, and info messages.
   *                    The default reporter is used if this is `null`.
   *  @param callback   Used to execute custom code during the compilation
   *                    process. No callbacks will be executed if this is `null`.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  final def process(args: Array[String], reporter: Reporter = null,
    callback: interfaces.CompilerCallback = null): Reporter = {
    val ctx = initCtx.fresh
    if (reporter != null)
      ctx.setReporter(reporter)
    if (callback != null)
      ctx.setCompilerCallback(callback)
    process(args, ctx)
  }

  /** Entry point to the compiler with no optional arguments.
   *
   *  This overload is provided for compatibility reasons: the
   *  `RawCompiler` of sbt expects this method to exist and calls
   *  it using reflection. Keeping it means that we can change
   *  the other overloads without worrying about breaking compatibility
   *  with sbt.
   */
  final def process(args: Array[String]): Reporter =
    process(args, null: Reporter, null: interfaces.CompilerCallback)

  /** Entry point to the compiler using a custom `Context`.
   *
   *  In most cases, you do not need a custom `Context` and should
   *  instead use one of the other overloads of `process`. However,
   *  the other overloads cannot be overridden, instead you
   *  should override this one which they call internally.
   *
   *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/compiler/test/dotty/tools/dotc/EntryPointsTest.scala.disabled]]
   *  in method `runCompilerWithContext`
   *
   *  @param args       Arguments to pass to the compiler.
   *  @param rootCtx    The root Context to use.
   *  @return           The `Reporter` used. Use `Reporter#hasErrors` to check
   *                    if compilation succeeded.
   */
  def process(args: Array[String], rootCtx: Context): Reporter = {
    val (fileNames, ictx) = setup(args, rootCtx)
    inContext(ictx) {
      doCompile(newCompiler, fileNames)
    }
  }

  def process(args: Array[String], srcs: Array[interfaces.incremental.SourceHandle], rootCtx: Context): Reporter =

    def doWarnIfIncremental()(using Context) =
      val addendum =
        if ctx.settings.fromTasty.value then
          s" Perhaps you did not mean to use the ${ctx.settings.fromTasty.name} compiler flag."
        else
          ""
      if ctx.incCallback != null then
        ctx.warning(s"Invoking the compiler in a state where incremental compilation will not receive back the files it injected.$addendum")

    val (fileNames, ictx) = setupVirtual(args, srcs, rootCtx)

    inContext(ictx) {
      fileNames match
        case Left(classNames) =>
          doWarnIfIncremental()
          doCompile(newCompiler, classNames)
        case Right(srcs1) =>
          doCompileVirtual(newCompiler, srcs1)
    }

  end process

  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal
    sys.exit(if (process(args).hasErrors) 1 else 0)
  }

}
