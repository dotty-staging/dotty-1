package dotty.tools
package dotc
package refine

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

import dotty.tools.dotc.refine.{RefinementType, EventuallyRefinementType}
/** A refinement type. This is internally represented as an annotated type with
  * a @retains or @retainsByName annotation, but the extractor will succeed only
  * at phase CheckRefinements. That way, we can ignore refinements information
  * until phase CheckRefinements since it is wrapped in a plain annotation.
  */
object RefinementType:
  def apply(parent: Type, refinement: Refinement)(using Context): Type =
    AnnotatedType(parent, RefinedAnnotation(refinement))

  /** An extractor that succeeds only during CheckRefinements.
    */
  def unapply(tp: Type)(using Context): Option[(Type, Refinement)] =
    if ctx.phase == Phases.checkRefinementsPhase then
      EventuallyRefinementType.unapply(tp)
    else None

/** An extractor for types that will be refinement types at phase
  * CheckRefinements.
  */
object EventuallyRefinementType:
  def unapply(tp: Type)(using Context): Option[(Type, Refinement)] =
    tp match
      case tp: AnnotatedType if tp.annot.symbol == defn.RefinedAnnot =>
        tp.annot match
          case RefinedAnnotation(refinement) => Some((tp.parent, refinement))
          case _ => Some((tp.parent, Refinement.Leaf(tp.annot.argument(0).get)))
      case _ => None
