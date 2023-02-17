package dotty.tools
package dotc
package refine

import core.*
import Annotations.*, Types.*, Symbols.*, Contexts.*, ast.tpd
import scala.collection.mutable
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.Text

case class RefinedAnnotation(refinement: Refinement) extends Annotation:
  override def tree(using Context): tpd.Tree = ???

  override def symbol(using Context) = defn.RefinedAnnot

  override def derivedAnnotation(tree: tpd.Tree)(using Context): Annotation = this

  override def toText(printer: Printer): Text = f"with ${refinement.show(using printer.printerContext)}"

  override def show(using Context): String = ""
