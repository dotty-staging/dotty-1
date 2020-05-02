object CacheTest {
  import scala.annotation.internal.{local, entry}
  import lib._

  object A {
    def test[T](t: T): T = t

    @entry def foo() = { // error
      local(null) { u =>
        val x = test(0)
        val y = test(u)
        Pair(x, y)
      }
    }
  }

  object B {
    class C {
      def test: C = this
    }

    @entry def bar() = local(new C) { c1 => // error
      val c2 = new C
      val x = c2.test
      val y = c1.test
      Pair(x, y)
    }
  }

  object lib {
    case class Pair[A, B](a: A, b: B)
    def local[T, U](t: T)(thunk: (T @local) => U): U =
      thunk(t)
  }
}
