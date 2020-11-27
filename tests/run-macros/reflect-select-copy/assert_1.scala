import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    Term.of(cond).underlyingArgument match {
      case Apply(Select(lhs, op) as select, rhs :: Nil) =>
        val cond = Apply(Select.copy(select)(lhs, ">"), rhs :: Nil).asExprOf[Boolean]
        '{ scala.Predef.assert($cond) }
      case _ =>
        '{ scala.Predef.assert($cond) }
    }
  }
}