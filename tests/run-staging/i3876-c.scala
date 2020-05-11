import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    implicit def toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)

    def x(using s: Scope): s.Expr[Int] = '{3}

    def f3(using s: Scope): s.Expr[Int => Int] = '{
      val f: (x: Int) => Int = x => x + x
      f
    }
    def expr(using Scope) = '{$f3($x)}
    println(run(Expr.betaReduce(expr)))
    println(usingNewScope(Expr.betaReduce(expr).show)) // TODO improve printer
  }
}
