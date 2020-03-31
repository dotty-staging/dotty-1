object WithFileTest {
  import scala.annotation.internal.local

  class SFile(path: String)

  // sanity check for the pos test

  def length(f: SFile) = f

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  def main(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      length(f)
    }

    withFile("") {
      f => // error
      val res = { () => length(f) }
      res()
    }
  }
}
