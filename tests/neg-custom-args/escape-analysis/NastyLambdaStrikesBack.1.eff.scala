object NastyLambdaStrikesBack {
  import scala.annotation.internal.{local, entry}
  import lib._

  @entry def foo(): Unit =
    local(0) { i =>
      val c = Cell[Int]()
      bar(c, () => i)
    }

  def bar(c: Cell[Int], f: () => Int): Cell[Int] = {
    c.value = f() // error
    c
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
