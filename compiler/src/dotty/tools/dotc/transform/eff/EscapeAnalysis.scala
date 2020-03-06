package dotty.tools
package dotc
package transform
package eff

import core._

import ast.tpd._
import core.Contexts.Context

import transform.MegaPhase.MiniPhase

class EscapeAnalysis extends MiniPhase {
  import EscapeAnalysis.{given _, _}

  val phaseName = EscapeAnalysis.name

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    if (!ctx.settings.YescapeAnalysis.value) return tree
    val Apply(fun, args) = tree
    val fsym = fun.symbol
    if fsym.exists && fsym.hasAnnotation(ctx.definitions.LocalParamsAnnot) then
      val engine = new EscapeAnalysisEngine(ctx)
      engine.analyseToplevel(tree)
    tree
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context): Tree = {
    // debug.println(i"{{{\n# template =\n$tree}}}")
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {
    if (!ctx.settings.YescapeAnalysis.value) return tree
    val engine = new EscapeAnalysisEngine(ctx)
    engine.analyseDefinitionWithLocalParameters(tree)
    tree
  }
}

object EscapeAnalysis {
  val name = "EscapeAnalysis"
}
