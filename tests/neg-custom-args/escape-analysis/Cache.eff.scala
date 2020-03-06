object CacheTest {
  import scala.annotation.internal.local

  case class Pair[A, B](a: A, b: B)

  object A {
    def test(i: Int): Int = i

    def foo(
      @local i: Int // error
    ): Pair[Int, Int] = {
      val x = test(0)
      val y = test(i)
      Pair(x, y)
    }
  }

  object B {
    class C {
      def test: C = this
    }

    def foo(
      @local c1: C // error
    ): Pair[C, C] = {
      val c2 = new C
      val x = c2.test
      val y = c1.test
      Pair(x, y)
    }
  }
}
