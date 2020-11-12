package scala.internal.quoted

import scala.quoted.{Expr, Quotes, Type}

object Unpickler {

  type PickledQuote = List[String]
  type PickledArgs = Seq[Seq[Any] => Any/*(([QCtx <: Quotes] =>> QCtx ?=> Expr[Any]) | Type[_])*/]

  def unpickleExpr[T](repr: PickledQuote, args: PickledArgs): Quotes ?=> Expr[T] =
    throw new Exception("Non bootstrapped lib")

  def unpickleType[T](repr: PickledQuote, args: PickledArgs): Quotes ?=> Type[T] =
    throw new Exception("Non bootstrapped lib")

}
