package scala.quoted

import scala.internal.quoted.PickledQuote

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API metaprogramming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: QuoteContext) = { import qctx.reflect._; ... }`.
 */
trait QuoteContext { self =>

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   */
  protected def unpickleExpr(pickledQuote: PickledQuote): scala.quoted.Expr[Any]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   */
  protected def unpickleType(pickledQuote: PickledQuote): scala.quoted.Type[?]

  /** Pattern matches the scrutinee against the pattern and returns a tuple
   *  with the matched holes if successful.
   *
   *  Examples:
   *    - `termMatch(< f(0, myInt) >, < f(0, myInt) >)`
   *       will return `Some(())` (where `()` is a tuple of arity 0)
   *    - `termMatch(< f(0, myInt) >, < f(patternHole[Int], patternHole[Int]) >)`
   *       will return `Some(Tuple2('{0}, '{ myInt }))`
   *    - `termMatch(< f(0, "abc") >, < f(0, patternHole[Int]) >)`
   *       will return `None` due to the missmatch of types in the hole
   *
   *  Holes:
   *    - scala.internal.Quoted.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
   *                                            if `U <:< T` and returns `x` as part of the match.
   *
   *  @param scrutinee `Expr` on which we are pattern matching
   *  @param pattern `Expr` containing the pattern tree
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Term``
   */
  protected def exprMatch(scrutinee: scala.quoted.Expr[Any], pattern: scala.quoted.Expr[Any]): Option[Tuple]

  /** Pattern matches the scrutineeType against the patternType and returns a tuple
   *  with the matched holes if successful.
   *
   *  @param scrutinee `Type` on which we are pattern matching
   *  @param pattern `Type` containing the pattern tree
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `scala.quoted.Type[Ti]``
   */
  protected def typeMatch(scrutinee: scala.quoted.Type[?], pattern: scala.quoted.Type[?]): Option[Tuple]

  /** Low-level Typed AST API metaprogramming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   */
  val reflect: scala.tasty.Reflection

  /** Type of a QuoteContext provided by a splice within a quote that took this context.
   *  It is only required if working with the reflection API.
   *
   *  Usually it is infered by the quotes an splices typing. But sometimes it is necessary
   *  to explicitly state that a context is nested as in the following example:
   *
   *  ```scala
   *  def run(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Unit =
   *    def nested()(using qctx.Nested): Expr[Int] = '{  ${ makeExpr(tree) } + 1  }
   *    '{  ${ nested() } + 2 }
   *  def makeExpr(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Expr[Int] = ???
   *  ```
   */
  type Nested = QuoteContext {
    val reflect: self.reflect.type
  }

}
