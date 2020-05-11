import scala.quoted._

object scalatest {
  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(using s: Scope)(condition: s.Expr[Boolean]): s.Expr[Unit] = {
    import s.tasty._
    val tree = condition
    def exprStr: String = condition.show

    tree.underlyingArgument match {
      case Apply(Select(lhs, op), rhs :: Nil) =>
        val left = lhs.seal
        val right = rhs.seal
        op match {
          case "===" =>
            '{
              val _left   = $left
              val _right  = $right
              val _result = _left == _right
              scala.Predef.assert(_result)
            }
        }
    }
  }
}
