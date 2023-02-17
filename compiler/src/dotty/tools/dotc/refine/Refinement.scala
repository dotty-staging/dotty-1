package dotty.tools
package dotc
package refine

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*
import util.Property.Key

import scala.collection.mutable
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.Text

enum Refinement extends Showable:
  case True
  case Var(i: Int)
  case Leaf(tree: Tree)
  case And(left: Refinement, right: Refinement)
  case Or(left: Refinement, right: Refinement)

  override def toText(printer: Printer): Text = ???

  override def show(using Context): String =
    this match
      case True => "true"
      case Var(i) => f"?p$i"
      // TODO(mbovel): remove this case
      case Leaf(Block(DefDef(_, List(params), rt, Apply(fn, args)) :: Nil, Closure(Nil, _, _)) ) =>
        fn.show
      case Leaf(tree) => tree.show
      case And(left, right) => f"${left.show} and ${right.show}"
      case Or(left, right) => f"${left.show} or ${right.show}"

import Refinement.*

import dotty.tools.dotc.refine.RefinementType
def and(left: Refinement, right: Refinement): Refinement =
  // if left == False || right == False then False else
  if left == True then right
  else if right == True then left
  else And(left, right)

def or(left: Refinement, right: Refinement): Refinement =
  if left == True || right == True then True
  // else if left == False then right
  // else if right == False then left
  else Or(left, right)

object Refinement:
  private val cache = mutable.HashMap[Type, Refinement]()

  def ofType(tp: Type)(using Context): Refinement  =
    //cache.getOrElseUpdate(tp, computeOfType(tp))
    val res = computeOfType(tp)
    //println(f"ofType(${tp.show}) == ${res.show}")
    res

  private def computeOfType(tp: Type)(using Context): Refinement =
    import Refinement.*
    tp.dealias match
      case RefinementType(parent, refinement) => and(ofType(parent), refinement)
      case AndType(tp1, tp2) => and(ofType(tp1), ofType(tp2))
      case OrType(tp1, tp2) => or(ofType(tp1), ofType(tp2))
      case tp: TypeProxy => ofType(tp.underlying)
      case _ => True
