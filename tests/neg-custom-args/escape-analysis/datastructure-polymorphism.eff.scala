object Test {
  import annotation.internal.{local, entry}
  import lib._
  import defs._

  def mapBox[T, S](b: Box[T], f: T => S): Box[S] =
    b map f

  @entry def foo() = { // error
    val box = LazyBox(() => ())
    withConsole { c =>
      mapBox(box, { u =>
        c.println(u)
        ()
      })
    }
  }

  @entry def bar() = {
    val box = StrictBox(())
    withConsole { c =>
      mapBox(box, { u =>
        c.println(u)
        ()
      })
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

    sealed trait Option[+T]
    final case class Some[+T](value: T) extends Option[T]
    case object None extends Option[Nothing]

    class Cell[T] { var value: T = _ }

    def local[T, U](t: T)(thunk: (T @local) => U): U =
      thunk(t)
  }
}
