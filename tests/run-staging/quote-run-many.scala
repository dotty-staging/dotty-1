import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def expr(i: Int)(using Scope) = '{
      val a = 3 + ${Expr(i)}
      2 + a
    }
    for (i <- 0 to 100)
      run(expr(i))
  }
}
