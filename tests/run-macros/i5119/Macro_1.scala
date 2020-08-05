import scala.quoted._

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ${ Macro.impl('sc, 'args) }
  }
  implicit inline def XmlQuote(inline sc: StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    Expr(sc.asTerm.underlyingArgument.showExtractors + "\n" + args.asTerm.underlyingArgument.showExtractors)
  }
}
