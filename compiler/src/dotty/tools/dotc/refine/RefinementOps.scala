package dotty.tools
package dotc
package refine

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*


extension (tp: Type)
  /** @pre `tp` is a RefinementType */
  def derivedRefinementType(parent: Type, refinement: refine.Refinement)(using Context): Type =
    tp match
      case tp @ RefinementType(p, r) =>
        if (parent eq p) && (refinement eq r) then tp
        else RefinementType(parent, refinement)

  def stripRefinements(using Context): Type =
    stripRefinementsMap(tp)

private def stripRefinementsMap(using Context) = new TypeMap:
  override def apply(tp: Type): Type =
    tp.dealias match
      case tp: AppliedType =>
        derivedAppliedType(tp, this(tp.tycon), tp.args)
      case tp: RefinedType =>
        derivedRefinedType(tp, this(tp.parent), tp.refinedInfo)
      case RefinementType(parent, refinement) =>
        this(parent)
      case _ =>
        mapOver(tp)
