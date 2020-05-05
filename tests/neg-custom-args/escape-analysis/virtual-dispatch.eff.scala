object test {
  import scala.annotation.internal.{local, entry}
  import lib._
  import defs._

  object case_straight {
    @entry def foo: Box[Int] = withConsole { c =>
      val v: Cell[Box[Int]] = Cell()
      v.value = StrictBox(0)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value
    }

    @entry def bar(): Box[Int] = withConsole { c => // error
      val v: Cell[Box[Int]] = Cell()
      v.value = StrictBox(0)
      v.value = LazyBox(() => 0)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }

    @entry def bar_crazy(): Box[Int] = withConsole { c => // error
      val v: Cell[Box[Int]] = Cell()
      v.value = StrictBox(0)
      def vv = {
        v.value = LazyBox(() => 0)
        v
      }
      vv.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }

    @entry def baz(): Box[Int] = withConsole { c =>
      val v: Cell[Box[Int]] = Cell()
      v.value = StrictBox(0)
      val res = v.value.map { i =>
        c.println(i)
        i
      }
      v.value = LazyBox(() => 0)
      res
    }

    // interestingly, this succeeds
    @entry def baz_alt(): Box[Int] = withConsole { c =>
      val v: Cell[Box[Int]] = Cell()
      v.value = StrictBox(0)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value = LazyBox(() => 0)
      v.value
    }
  }

  object case_method {
    @entry def foo(): Box[Int] = withConsole { c =>
      val v: Cell[Box[Int]] = Cell()
      foo_set(v)
      v.value
    }

    def foo_set(v: Cell[Box[Int]]): Unit = {
      v.value = StrictBox(0)
    }

    @entry def bar(): Box[Int] = withConsole { c => // error
      val v: Cell[Box[Int]] = Cell()
      bar_set(v)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }

    def bar_set(v: Cell[Box[Int]]): Unit = {
      v.value = StrictBox(0)
      v.value = LazyBox(() => 0)
    }
  }

  object case_method_alt {
    @entry def foo(): Box[Int] = withConsole { c =>
      val v: Cell[Box[Int]] = Cell()
      foo_impl(v)(c)
      v.value
    }

    def foo_impl(v: Cell[Box[Int]])(c: Console): Unit = {
      v.value = StrictBox(0)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
    }

    @entry def bar(): Box[Int] = withConsole { c => // error
      val v: Cell[Box[Int]] = Cell()
      bar_impl(v)(c)
      v.value
    }

    def bar_impl(v: Cell[Box[Int]])(c: Console): Unit = {
      v.value = StrictBox(0)
      v.value = LazyBox(() => 0)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
    }
  }

  object case_local {
    @entry def foo(): Box[Int] = withConsole { c =>
      def foo_set(v: Cell[Box[Int]]): Unit = {
        v.value = StrictBox(0)
      }

      val v: Cell[Box[Int]] = Cell()
      foo_set(v)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value
    }

    @entry def bar(): Box[Int] = withConsole { c => // error
      def bar_set(v: Cell[Box[Int]]): Unit = {
        v.value = StrictBox(0)
        v.value = LazyBox(() => 0)
      }

      val v: Cell[Box[Int]] = Cell()
      bar_set(v)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }
  }

  object case_lambda {
    @entry def foo(): Box[Int] = withConsole { c =>
      val foo_set: (v: Cell[Box[Int]]) => Unit =
        (v: Cell[Box[Int]]) => {
          v.value = StrictBox(0)
        }

      val v: Cell[Box[Int]] = Cell()
      foo_set(v)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value
    }

    @entry def bar(): Box[Int] = withConsole { c => // error
      val bar_set: (v: Cell[Box[Int]]) => Unit =
        (v: Cell[Box[Int]]) => {
          v.value = StrictBox(0)
          v.value = LazyBox(() => 0)
        }

      val v: Cell[Box[Int]] = Cell()
      bar_set(v)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }
  }

  object defs {
    trait Console {
      def println(a: Any): Unit
    }

    def withConsole[T](thunk: (Console @local) => T): T = {
      val c = new Console { def println(a: Any) = () }
      thunk(c)
    }

    trait Box[T] {
      def map[U](f: T => U): Box[U]
    }

    class StrictBox[T](t: T) extends Box[T] {
      def map[U](f: T => U): StrictBox[U] =
        StrictBox(f(t))
    }

    class LazyBox[T](val thunk: () => T) extends Box[T] {
      def map[U](f: T => U): LazyBox[U] =
        LazyBox(() => f(thunk()))
    }
  }

  object lib {
    case class Pair[A, B](a: A, b: B)

    class Cell[T] { var value: T = _ }

    def local[T, U](t: T)(thunk: (T @local) => U): U =
      thunk(t)
  }
}
