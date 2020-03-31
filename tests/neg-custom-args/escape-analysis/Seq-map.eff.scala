object SeqTest {
  import scala.annotation.internal.local

  sealed trait List2[T]
  final case class Nil2[T]() extends List2[T]
  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  object List2 {
    def map[T, U](list: List2[T], @local f: T => U): List2[U] =
      list match {
        case Cons2(head, rest) =>
          Cons2(f(head), map(rest, f))
        case Nil2() =>
          Nil2()
      }

    def foo(
      @local x: Int // error
    ): List2[Int] = {
      val l1 = map(Cons2(0, Nil2()), i => x)
      val l2 = map(Cons2(0, Nil2()), i => 1)
      l1
    }

    // def control(
    //   @local x: Int
    // ): List2[Int] = {
    //   val l1 = map(Cons2(0, Nil2()), i => x)
    //   val l2 = map(Cons2(0, Nil2()), i => 1)
    //   l2
    // }
  }
}
