object NastyLambda {
  import scala.annotation.internal.{local, entry}
  import lib._

  @entry def foo(): Int = // error
    local(0) { i =>
      bar(() => i)
    }

  def bar(f: () => Int): Int = {
    f()
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
