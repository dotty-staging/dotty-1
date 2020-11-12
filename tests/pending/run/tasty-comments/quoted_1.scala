import scala.quoted._


object Macros {

  inline def printComment[T](t: => T): Unit =
    ${ impl('t) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val tree = x.asReflectTree
    tree.symbol.comment.map(_.raw) match {
      case Some(str) => '{ println(${str}) }
      case None => '{ println() }
    }
  }
}
