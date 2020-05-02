object SeqTest {
  import scala.annotation.internal.{local, entry}
  import lib._

  sealed trait List2[T]
  final case class Nil2[T]() extends List2[T]
  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  object List2 {
    def map[T, U](list: List2[T], f: T => U): List2[U] =
      list match {
        case Cons2(head, rest) =>
          Cons2(f(head), map(rest, f))
        case Nil2() =>
          Nil2()
      }

    @entry def foo(): List2[Int] = // error
      local(0) {  x =>
        val l1 = map(Cons2(0, Nil2()), i => x)
        val l2 = map(Cons2(0, Nil2()), i => 1)
        l1
      }

    @entry def bar(): List2[Int] =
      local(0) {  x =>
        val l1 = map(Cons2(0, Nil2()), i => x)
        val l2 = map(Cons2(0, Nil2()), i => 1)
        l2
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
