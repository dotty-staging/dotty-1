package scala.internal.quoted

import scala.quoted.QuoteContext
import scala.tasty.reflect._
import scala.internal.quoted.PickledQuote

/** Part of the QuoteContext interface that needs to be implemented by the compiler but is not visible to users */
trait QuoteContextInternal extends QuoteContext {
  def unpickleExpr(pickledQuote: PickledQuote): scala.quoted.Expr[Any]
  def unpickleType(pickledQuote: PickledQuote): scala.quoted.Type[?]
  def exprMatch(scrutinee: scala.quoted.Expr[Any], pattern: scala.quoted.Expr[Any]): Option[Tuple]
  def typeMatch(scrutinee: scala.quoted.Type[?], pattern: scala.quoted.Type[?]): Option[Tuple]
}
