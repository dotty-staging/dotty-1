import scala.quoted._

inline def test(inline e: Int): String = ${testExpr('e)}

private def testExpr(e: Expr[Int])(using Quotes): Expr[String] = {
  e match {
    case '{ val y: Int = 4; $body } => Value("Matched closed\n" + body.show)
    case '{ val y: Int = 4; $body(y): Int } => Value("Matched open\n" + body.show)
    case '{ val y: Int => Int = x => x + 1; $body(y): Int } => Value("Matched open\n" + body.show)
    case '{ def g(x: Int): Int = $body(g, x); g(5) } => Value("Matched open\n" + body.show)
  }
}
