object NestedMatch {
  import scala.annotation.internal.{local, entry}
  import lib._

  sealed trait List2[T]
  final case class Nil2[T]() extends List2[T]
  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  def foo(list: List2[Int]): Int = local(0) { cap =>
    list match {
      case Cons2(_, Cons2(0, _) | Nil2) => cap
      case Cons2(_, Cons2(h, _)) => h
      case Nil2 => 0
    }
  }

  @entry def foo1() = foo(Nil2()) // error
  @entry def foo2() = foo(Cons2(0, Nil2())) // error
  @entry def foo3() = foo(Cons2(0, Cons2(0, Nil2()))) // error
  @entry def foo4() = foo(Cons2(1, Cons2(0, Nil2()))) // error

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
