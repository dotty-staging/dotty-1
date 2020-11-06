package scala.quoted

import scala.internal.quoted.PickledQuote

trait QuoteContext { self =>

  private[scala] def unpickleExpr(pickledQuote: PickledQuote): scala.quoted.Expr[Any]
  private[scala] def unpickleType(pickledQuote: PickledQuote): scala.quoted.Type[?]
  private[scala] def exprMatch(scrutinee: scala.quoted.Expr[Any], pattern: scala.quoted.Expr[Any]): Option[Tuple]
  private[scala] def typeMatch(scrutinee: scala.quoted.Type[?], pattern: scala.quoted.Type[?]): Option[Tuple]

  val reflect: scala.tasty.Reflection

  type Nested = QuoteContext {
    val reflect: self.reflect.type
  }

}
