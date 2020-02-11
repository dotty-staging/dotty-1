object SeqTest {
  import scala.annotation.internal.local

  sealed trait List2[T] {
  }

  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  final case class Nil2[T]() extends List2[T]

  object List2 {
    def map[T, U](list: List2[T], @local f: T => U): List2[U] =
      list match {
        case Cons2(head, rest) =>
          Cons2(f(head), map(rest, f))
        case Nil2() =>
          Nil2()
      }
  }
}
