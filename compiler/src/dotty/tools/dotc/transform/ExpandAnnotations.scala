package dotty.tools.dotc
package transform

import core._
import Flags._
import Contexts._
import Symbols._
import SymUtils._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.TreeMapWithImplicits
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.config.Printers.{macroAnnot => debug}
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.MacroClassLoader
import dotty.tools.dotc.core.StagingContext.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.SymDenotations.NoDenotation
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.quoted.*
import dotty.tools.dotc.util.SrcPos
import scala.quoted.runtime.impl.{QuotesImpl, SpliceScope}

/** Expand TASTy macro annotation
 *
 *  Transforms annotated definitions starting in top to bottom order.
 */
class ExpandAnnotations extends MacroTransform with IdentityDenotTransformer {
  import tpd.*
  import TastyAnnotations.*

  override def phaseName: String = ExpandAnnotations.name

  override def description: String = ExpandAnnotations.description

  override def allowsImplicitSearch: Boolean = true

  override def changesMembers: Boolean = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.hasTastyAnnotations then
      try super.run
      catch case _: CompilationUnit.SuspendException => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val newUnits = super.runOn(units).filterNot(_.suspended)
    ctx.run.nn.checkSuspendedUnits(newUnits)
    newUnits

  def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      new ExpandTastyAnnotationTreeMap().transform(tree)
  }

  private class ExpandTastyAnnotationTreeMap extends TreeMapWithImplicits {
    override def transform(tree: Tree)(using Context): Tree = {
      tree match
        case tree: MemberDef =>
          if tree.symbol.is(Inline) then tree
          else if tree.symbol.is(Param) then super.transform(tree)
          else if
            !tree.symbol.isPrimaryConstructor
            && level == 0
            && hasTastyAnnotation(tree.symbol)
          then
            val trees = new TastyAnnotations(ExpandAnnotations.this).expandAnnotations(tree)
            flatTree(trees.map(super.transform))
          else super.transform(tree)
        case _: RefTree if tree.symbol.is(Inline) && !Inlines.inInlineMethod =>
          ctx.compilationUnit.needsInlining = true
          super.transform(tree)
        case _: GenericApply if tree.symbol.isQuote =>
          super.transform(tree)(using quoteContext)
        case _: GenericApply if tree.symbol.isExprSplice =>
          super.transform(tree)(using spliceContext)
        case _ =>
          super.transform(tree)
    }
  }

}

object ExpandAnnotations:
  val name: String = "expand-annotations"
  val description: String = "Expands TASTy annotations"
