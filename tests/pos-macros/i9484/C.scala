import scala.quoted._

object C {
  inline def m: Any = ${ mExpr }
  def mExpr(using s: Scope): s.Expr[Any] = Expr(1)
}
