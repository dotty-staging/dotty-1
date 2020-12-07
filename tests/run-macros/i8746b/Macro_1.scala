
import scala.quoted._

object Macro {
  inline def mac(inline tree: Any): String = ${ macImpl('tree) }
  def macImpl(tree: Expr[Any])(using Quotes): Expr[String] = {
    tree match {
        case '{ (in: tpe1) => ($out: tpe2) } => Value(out.toString)
        case _ => Value("not matched")
    }
  }
}
