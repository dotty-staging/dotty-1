package scala.tasty
package reflect

import scala.quoted.QuoteContext

trait Printer[QCtx <: QuoteContext & Singleton] {

  /** Instance of reflection interface */
  val qctx: QCtx
  import qctx.tasty._

  /** Show a String representation of a qctx.tasty.Tree */
  def showTree(tree: Tree): String

  /** Show a String representation of a qctx.tasty.Type */
  def showType(tpe: Type): String

  /** Show a String representation of a qctx.tasty.Constant */
  def showConstant(const: Constant): String

  /** Show a String representation of a qctx.tasty.Symbol */
  def showSymbol(symbol: Symbol): String

  /** Show a String representation of a qctx.tasty.Flags */
  def showFlags(flags: Flags): String
}
