import scala.quoted._

trait Show[T] {
  def show(x: T): String
}

def showInterpolatorImpl(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using qctx: QuoteContext): Expr[String] =
  argsExpr.asTerm match
    case '{ $arg: $t } => // error
    case '[ Int ] => // error
  ???

extension (inline sc: StringContext) inline def show (args: Any*): String = ${ showInterpolatorImpl('sc, 'args) }
