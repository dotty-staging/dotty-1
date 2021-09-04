package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*

object CapturingType:

  def apply(parent: Type, refs: CaptureSet)(using Context): Type =
    if refs.isAlwaysEmpty then parent
    else AnnotatedType(parent, CaptureAnnotation(refs))

  def unapply(tp: AnnotatedType)(using Context) =
    if ctx.phase == Phases.checkCapturesPhase && tp.annot.symbol == defn.RetainsAnnot then
      tp.annot match
        case ann: CaptureAnnotation => Some((tp.parent, ann.refs))
        case ann => Some((tp.parent, ann.tree.toCaptureSet))
    else None

end CapturingType
