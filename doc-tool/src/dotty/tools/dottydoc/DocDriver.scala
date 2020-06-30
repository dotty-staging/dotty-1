package dotty.tools
package dottydoc

import java.io.File

import dotty.tools.dottydoc.util.syntax._
import core.ContextDottydoc
import dotc.core.Contexts._
import dotc.reporting.Reporter
import dotc.{ Compiler, Driver }
import dotc.config._
import dotc.core.Comments.ContextDoc
import dotc.interfaces
import staticsite.Site

/** `DocDriver` implements the main entry point to the Dotty documentation
 *  tool. It's methods are used by the external scala and java APIs.
 */
class DocDriver extends Driver {
  import java.util.{ Map => JMap }
  import model.JavaConverters._

  private def setupCommon[T](args: Array[String], rootCtx: Context): (Settings.ArgsSummary, Context) =
    val ctx     = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)
    ctx.setSettings(summary.sstate)
    ctx.setSetting(ctx.settings.YcookComments, true)
    ctx.setSetting(ctx.settings.YnoInline, true)
    ctx.setProperty(ContextDoc, new ContextDottydoc)
    (summary, ctx)
  end setupCommon

  override def setupVirtual(args: Array[String], rootCtx: Context): Context =
    val (summary, ictx) = setupCommon(args, rootCtx)

    implicit val ctx: Context = ictx

    val fileNames = CompilerCommand.checkUsage(summary, false)
    assert(fileNames.isEmpty)
    ctx
  end setupVirtual

  override def setup(args: Array[String], rootCtx: Context): (List[String], Context) =
    val (summary, ictx) = setupCommon(args, rootCtx)

    implicit val ctx: Context = ictx

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    fromTastySetup(fileNames, ctx)
  end setup

  override def newCompiler(implicit ctx: Context): Compiler = new DocCompiler

  def processFinal(reporter: Reporter)(using ctx: Context): Reporter = {
    val siteRoot = File(ctx.settings.siteRoot.value)
    val projectName = ctx.settings.projectName.value
    val projectVersion = ctx.settings.projectVersion.value
    val projectUrl = Option(ctx.settings.projectUrl.value).filter(_.nonEmpty)
    val projectLogo = Option(ctx.settings.projectLogo.value).filter(_.nonEmpty)
    val docSnapshot = ctx.settings.docSnapshot.value

    val baseUrl = ""
    val outDir = File(siteRoot, "_site")
    val snapshotFolderName = if projectVersion.endsWith("NIGHTLY") then "nightly" else projectVersion
    val snapshotOutDir = File(outDir, snapshotFolderName)
    val snapshotBaseUrl = s"$baseUrl/$snapshotFolderName"

    if (projectName.isEmpty)
      ctx.error(s"Site project name not set. Use `-project <title>` to set the project name")
    else if (!siteRoot.exists || !siteRoot.isDirectory)
      ctx.error(s"Site root does not exist: $siteRoot")
    else {
      def generateSite(outDir: File, baseUrl: String) =
        Site(siteRoot, outDir, projectName, projectVersion, projectUrl, projectLogo, ctx.docbase.packages, baseUrl)
          .generateApiDocs()
          .copyStaticFiles()
          .generateHtmlFiles()
          .generateBlog()

      generateSite(outDir, baseUrl)
      if docSnapshot then generateSite(snapshotOutDir, snapshotBaseUrl)
      ctx.docbase.printSummary()
    }

    reporter
  }

  override def process(args: Array[String], rootCtx: Context): Reporter = {
    val (filesToDocument, ictx) = setup(args, initCtx.fresh)

    implicit val ctx: Context = ictx
    val reporter = doCompile(newCompiler, filesToDocument)

    processFinal(reporter)
  }

  override def process(args: Array[String], srcs: Array[interfaces.incremental.SourceHandle], rootCtx: Context): Reporter =
    implicit val ictx: Context = setupVirtual(args, initCtx.fresh)
    if (ctx.settings.fromTasty.value)
      val (virtual, real) = srcs.partitionMap { src =>
        val p = src.pathOrNull
        if p == null then Left(src)
        else Right(p.toString)
      }
      if virtual.nonEmpty then // TODO: we should be able to adapt fromTastySetup to work with just InputStream and name
        ctx.error(s"-from-tasty can only be used with real files, the following sources are virtual:\n${virtual.mkString(", ")}")
        ctx.reporter
      else
        val (srcs0, ictx) = fromTastySetup(real.toList, ctx)
        processFinal(doCompile(newCompiler, srcs0)(ictx))
    else
      processFinal(doCompileVirtual(newCompiler, srcs.toList))
}
