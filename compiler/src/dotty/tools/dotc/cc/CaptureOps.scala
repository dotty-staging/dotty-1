package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.capt
import util.Property.Key
import tpd.*

private val Captures: Key[CaptureSet] = Key()

def retainedElems(tree: Tree)(using Context): List[Tree] = tree match
  case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) => elems
  case _ => Nil

extension (tree: Tree)

  def toCaptureRef(using Context): CaptureRef = tree.tpe.asInstanceOf[CaptureRef]

  def toCaptureSet(using Context): CaptureSet =
    tree.getAttachment(Captures) match
      case Some(refs) => refs
      case None =>
        val refs = CaptureSet(retainedElems(tree).map(_.toCaptureRef)*)
          .showing(i"toCaptureSet $tree --> $result", capt)
        tree.putAttachment(Captures, refs)
        refs

extension (tp: Type)

  def derivedCapturingType(parent: Type, refs: CaptureSet)(using Context): Type = tp match
    case CapturingType(p, r) =>
      if (parent eq p) && (refs eq r) then tp
      else CapturingType(parent, refs)

  /** If this is  type variable instantiated or upper bounded with a capturing type,
   *  the capture set associated with that type. Extended to and-or types and
   *  type proxies in the obvious way. If a term has a type with a boxed captureset,
   *  that captureset counts towards the capture variables of the envirionment.
   */
  def boxedCaptured(using Context): CaptureSet =
    def getBoxed(tp: Type, enabled: Boolean): CaptureSet = tp match
      case CapturingType(_, refs) if enabled => refs
      case tp: TypeVar => getBoxed(tp.underlying, enabled = true)
      case tp: TypeRef if tp.symbol == defn.AnyClass && enabled => CaptureSet.universal
      case tp: TypeProxy => getBoxed(tp.superType, enabled)
      case tp: AndType => getBoxed(tp.tp1, enabled) ++ getBoxed(tp.tp2, enabled)
      case tp: OrType => getBoxed(tp.tp1, enabled) ** getBoxed(tp.tp2, enabled)
      case _ => CaptureSet.empty
    getBoxed(tp, enabled = false)

  /** If this type appears as an expected type of a term, does it imply
   *  that the term should be boxed?
   *  ^^^ Special treat Any? - but the current status is more conservative in that
   *  it counts free variables in expressions that have Any as expected type.
   */
  def needsBox(using Context): Boolean = tp match
    case _: TypeVar => true
    case tp: TypeRef =>
      tp.info match
        case TypeBounds(lo, _) => lo.needsBox
        case _ => false
    case tp: RefinedOrRecType => tp.parent.needsBox
    case CapturingType(_, _) => false
    case tp: AnnotatedType => tp.parent.needsBox
    case tp: LazyRef => tp.ref.needsBox
    case tp: AndType => tp.tp1.needsBox || tp.tp2.needsBox
    case tp: OrType => tp.tp1.needsBox && tp.tp2.needsBox
    case _ => false

  def canHaveInferredCapture(using Context): Boolean = tp match
    case tp: TypeRef if tp.symbol.isClass =>
      !tp.symbol.isValueClass && tp.symbol != defn.AnyClass
    case tp: TypeProxy =>
      tp.superType.canHaveInferredCapture
    case tp: AndType =>
      tp.tp1.canHaveInferredCapture && tp.tp2.canHaveInferredCapture
    case tp: OrType =>
      tp.tp1.canHaveInferredCapture || tp.tp2.canHaveInferredCapture
    case _ =>
      false

  def stripCapturing(using Context): Type = tp.dealiasKeepAnnots match
    case CapturingType(parent, _) =>
      parent.stripCapturing
    case atd @ AnnotatedType(parent, annot) =>
      atd.derivedAnnotatedType(parent.stripCapturing, annot)
    case _ =>
      tp
