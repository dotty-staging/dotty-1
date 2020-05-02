object NastyLambdaStrikesBack {
  import scala.annotation.internal.{local, entry}
  import lib._

  @entry def foo() = local (null) { v =>
    val c = Cell[v.type]()
    bar(c, () => v)
  }

  def bar[T](c: Cell[T], @local f: () => T) = {
    c.value = f() // error
    c
  }

  object lib {
    case class Pair[A, B](a: A, b: B)

    class Cell[T] { var value: T = _ }

    def local[T, U](t: T)(thunk: (T @local) => U): U =
      thunk(t)
  }
}
