import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def expr(i: Int)(using Quotes) = '{
      val a = 3 + ${Value(i)}
      2 + a
    }
    for (i <- 0 to 50)
      run(expr(i))
  }
}
