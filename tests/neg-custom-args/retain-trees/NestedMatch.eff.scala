object NestedMatch {
  import annotation.internal.local

  sealed trait List2[T]
  final case class Nil2[T]() extends List2[T]
  final case class Cons2[T](
    head: T,
    tail: List2[T]
  ) extends List2[T]

  def foo(@local cap: String, list: List2[Int]): String = {
    list match {
      case Cons2(_, tree) =>
        val inner =
          tree match {
            case Cons2(_, _) => cap
            case Nil2 => ""
          }

        op(cap)
      case Nil2 => ""
    }
  }

  def foo_control(
    @local cap: String, // error
    list: List2[Int]
  ): String = {
    list match {
      case Cons2(_, tree) =>
        val inner =
          tree match {
            case Cons2(_, _) => cap
            case Nil2 => ""
          }

        inner
      case Nil2 => ""
    }
  }

  def op(@local cap: String): String = ""
}
