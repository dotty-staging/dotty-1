object NestedMatch {
  import scala.annotation.internal.{local, entry}
  import lib._

  sealed trait List2[T]
  final case class Nil2[T]() extends List2[T]
  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  def foo(list: List2[Int]): String = local("") { cap =>
    list match {
      case Cons2(_, tree) =>
        val inner =
          tree match {
            case Cons2(_, _) => cap
            case Nil2 => ""
          }

        op(cap)
      case Nil2() => ""
    }
  }

  @entry def foo1() = foo(Nil2())
  @entry def foo2() = foo(Cons2(0, Nil2()))
  @entry def foo3() = foo(Cons2(0, Cons2(0, Nil2())))

  def bar(list: List2[Int]): String = local("") { cap =>
    list match {
      case Cons2(_, tree) =>
        val inner =
          tree match {
            case Cons2(_, _) => cap
            case Nil2 => ""
          }

        inner
      case Nil2() => ""
    }
  }

  @entry def bar1() = bar(Nil2()) // error
  @entry def bar2() = bar(Cons2(0, Nil2())) // error
  @entry def bar3() = bar(Cons2(0, Cons2(0, Nil2()))) // error

  def op(cap: String): String = ""

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
