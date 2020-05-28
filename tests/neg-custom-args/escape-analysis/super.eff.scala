object Test {
  import annotation.internal.{entry, local}
  import lib._

  class BoxParent[T](value: T) {
    def unwrap = value
  }

  class Box[T](value: T) extends BoxParent(value) {
    override def unwrap = super[BoxParent].unwrap
  }

  @entry def foo(): Unit = { // error
    local(null) { u =>
      val box = Box(u)
      box.unwrap
    }
  }
}

object lib {
  import annotation.internal.{entry, local}

  case class Pair[A, B](a: A, b: B)

  sealed trait Option[+T]
  final case class Some[+T](value: T) extends Option[T]
  case object None extends Option[Nothing]

  class Cell[T] { var value: T = _ }

  def local[T, U](t: T)(thunk: (T @local) => U): U =
    thunk(t)
}
