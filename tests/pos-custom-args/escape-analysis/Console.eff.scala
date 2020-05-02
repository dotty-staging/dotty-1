object ConsoleTest {
  import scala.annotation.internal.{local, entry}
  import lib._

  class IO
  class Console(io: IO)

  @entry def foo() =
    local(new Console(new IO)) { console =>
      bar(console)
    }

  def bar(console: Console) = {
    ()
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
