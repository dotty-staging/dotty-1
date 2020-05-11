
import scala.quoted._

object Macro {


  object Implementation {

    inline def plus(inline n: Int, m: Int): Int = ${ plus('n, 'm) }

    def plus(using s: Scope)(n: s.Expr[Int], m: s.Expr[Int]): s.Expr[Int] =
      if (n.unliftOrError == 0) m
      else '{ ${n} + $m }

    object Implementation2 {

      inline def plus(inline n: Int, m: Int): Int = ${ plus('n, 'm) }

      def plus(using s: Scope)(n: s.Expr[Int], m: s.Expr[Int]): s.Expr[Int] =
        if (n.unliftOrError == 0) m
        else '{ ${n} + $m }
    }
  }

}
