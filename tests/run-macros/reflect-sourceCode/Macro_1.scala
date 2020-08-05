import scala.quoted._

object api {
  extension [T](x: => T) inline def reflect: String =
    ${ reflImpl('x) }

  private def reflImpl[T](x: Expr[T])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    Expr(x.asTerm.pos.sourceCode)
  }
}
