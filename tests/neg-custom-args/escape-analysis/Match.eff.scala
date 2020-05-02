object MatchTest {
  import scala.annotation.internal.{local, entry}
  import lib._

  def foo(opt: Option[Int], x: Int): Int = {
    opt match {
      case Some(i) => i
      case None => 0
    }
  }

  @entry def foo_1() =
    local(0) { i => foo(Some(0), i) }

  @entry def foo_2() =
    local(0) { i => foo(None, i) }

  def bar(opt: Option[Int], x: Int): Int = {
    opt match {
      case Some(i) => x
      case None => 0
    }
  }

  @entry def bar_1() = // error
    local(0) { i => bar(Some(0), i) }

  @entry def bar_2() = // error
    local(0) { i => bar(None, i) }

  def baz(opt: Option[Int], x: Int): Int = {
    opt match {
      case Some(1) => x
      case Some(_) | None => 0
    }
  }

  @entry def baz_1() = // error
    local(0) { i => baz(Some(1), i) }

  @entry def baz_2() = // error
    local(0) { i => baz(None, i) }

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
