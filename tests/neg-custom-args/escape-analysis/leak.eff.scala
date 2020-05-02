object WithFileTest {
  import scala.annotation.internal.{local, entry}

  class SFile(path: String)

  // sanity check for the pos test

  def length(f: SFile) = f

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  @entry def foo(): Unit = { // error
    withFile("") {
      f =>
      length(f)
    }
  }

  @entry def bar(): Unit = { // error
    withFile("") {
      f =>
      val res = { () => length(f) }
      res()
    }
  }

  @entry def baz(): Unit =
    withFile("") { f => 0 }

}
