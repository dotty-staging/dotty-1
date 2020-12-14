import scala.quoted._

inline def printPos[T](inline expr: T): (Int, Int) =
  ${ printPos('expr) }

private def printPos[T](expr: Expr[T])(using Quotes): Expr[(Int, Int)] =
  import quotes.reflect._
  val pos = expr.asTerm.pos.get
  Expr((pos.start, pos.end))
