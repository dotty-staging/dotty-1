package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.Trees.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.capt
import printing.Printer
import printing.Texts.Text


case class CaptureAnnotation(refs: CaptureSet) extends Annotation:
  import CaptureAnnotation.*
  import tpd.*

  override def tree(using Context) =
    val elems = refs.elems.toList.map {
      case cr: TermRef => ref(cr)
      case cr: TermParamRef => untpd.Ident(cr.paramName).withType(cr)
      case cr: ThisType => This(cr.cls)
    }
    val arg = repeated(elems, TypeTree(defn.AnyType))
    New(symbol.typeRef, arg :: Nil)

  override def symbol(using Context) = defn.RetainsAnnot

  override def derivedAnnotation(tree: Tree)(using Context): Annotation =
    unsupported("derivedAnnotation(Tree)")

  def derivedAnnotation(refs: CaptureSet)(using Context): Annotation =
    if this.refs eq refs then this else CaptureAnnotation(refs)

  override def sameAnnotation(that: Annotation)(using Context): Boolean = that match
    case CaptureAnnotation(refs2) => refs == refs2
    case _ => false

  override def mapWith(tp: TypeMap)(using Context) =
    val elems = refs.elems.toList
    val elems1 = elems.mapConserve(tp)
    if elems1 eq elems then this
    else if elems1.forall(_.isInstanceOf[CaptureRef])
    then CaptureAnnotation(CaptureSet(elems1.asInstanceOf[List[CaptureRef]]*))
    else EmptyAnnotation

  override def refersToParamOf(tl: TermLambda)(using Context): Boolean =
    refs.elems.exists {
      case TermParamRef(tl1, _) => tl eq tl1
      case _ => false
    }

  override def toText(printer: Printer): Text = refs.toText(printer)

  override def hash: Int = refs.hashCode

  override def eql(that: Annotation) = that match
    case that: CaptureAnnotation => this.refs eq that.refs
    case _ => false

end CaptureAnnotation
