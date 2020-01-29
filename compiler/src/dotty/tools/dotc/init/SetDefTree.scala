package dotty.tools.dotc
package transform
package init

import MegaPhase._
import ast.tpd
import core.Contexts.Context

/** "Manually" sets the `Yretain-trees` option later on in the run.
  *
  * This phase is meant to run on its own _right before_ a phase that needs the definition trees of symbols.
  * Just setting `Yretain-trees` is not enough, since the trees retained during pickling are not correctly
  * updated during later phases. Instead, this phase does a complete pass over current trees and sets all
  * the definition trees right before they are needed, avoiding any staleness problems.
  */
class SetDefTree extends MiniPhase {
  import tpd._

  override val phaseName: String = SetDefTree.name
  override val runsAfter = Set(Pickler.name)

  override def isEnabled(implicit ctx: Context): Boolean =
    super.isEnabled

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val ctx2 = ctx.fresh.setSetting(ctx.settings.YretainTrees, true)
    super.runOn(units)(ctx2)
  }

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = tree.setDefTree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = tree.setDefTree

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = tree.setDefTree
}

object SetDefTree {
  val name: String = "SetDefTree"
}
