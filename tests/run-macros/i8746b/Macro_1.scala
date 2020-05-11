
import scala.quoted._

object Macro {
  inline def mac(inline tree: Any): String = ${ macImpl('tree) }
  def macImpl(using s: Scope)(tree: s.Expr[Any]): s.Expr[String] = {
    tree match {
        case '{ (in: $tpe1) => ($out: $tpe2) } => Expr(out.toString)
        case _ => Expr("not matched")
    }
  }
}
