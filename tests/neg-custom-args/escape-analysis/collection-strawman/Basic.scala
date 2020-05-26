object Basic {
  import scala.annotation.internal.{entry, local}
  import lib._

  import strawman.collections.Strawman._

  @entry def foo() = {
    local(()) { u =>
      val list = u :: u :: Nil
      val resu = list.map(u => ())
      resu
    }
  }

  @entry def bar() = {
    local(new Console {}) { c =>
      val list = 1 :: 2 :: Nil
      val resu = list.map { i =>
        c.println(i)
        i
      }
      resu
    }
  }

  object lib {
    case class Pair[A, B](a: A, b: B)

    class Cell[T] { var value: T = _ }

    def local[T, U](t: T)(thunk: (T @local) => U): U =
      thunk(t)

    trait Console {
      def println(a: Any): Unit = ()
    }
  }
}
