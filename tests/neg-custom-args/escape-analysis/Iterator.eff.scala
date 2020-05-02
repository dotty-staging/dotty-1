object IteratorTest {
  import scala.annotation.internal.{local, entry}

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

  @entry def foo(): Unit = { // error
    withFile("") {f =>
      val iter = new Iterator[Any] {
        def next() = ???
      }

      iter.map(_ => length(f))
    }
  }

  @entry def bar(): Unit = {
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
