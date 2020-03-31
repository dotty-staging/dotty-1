object IteratorTest {
  import scala.annotation.internal.local

  class SFile(path: String)

  def length(f: SFile): Int = 0

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
      thunk(f)
  }


  abstract class Iterator[A] {
    def next(): A
    def map[AA](f: A => AA): Iterator[AA] = {
      val outer = this
      new Iterator[AA] {
        def next() = f(outer.next())
      }
    }
  }

  def main(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      val iter = new Iterator[Any] {
        def next() = ???
      }

      iter.map(_ => length(f))
    }

    withFile("") {
      f =>
      val iter = new Iterator[Any] {
        def next() = ???
      }

      val iter2 =
        iter.map(_ => length(f))

      length(f)
    }

  }
}
