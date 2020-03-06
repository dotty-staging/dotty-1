object NestedMatch {
  import annotation.internal.local

  sealed trait List2[T]
  final case class Nil2[T]() extends List2[T]
  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  def foo(
    @local cap: Int, // error
    list: List2[Int]
  ): Int = {
    list match {
      case Cons2(_, Cons2(0, _) | Nil2) => cap
      case Cons2(_, Cons2(h, _)) => h
      case Nil2 => 0
    }
  }

  def op(@local cap: String): String = ""
}
