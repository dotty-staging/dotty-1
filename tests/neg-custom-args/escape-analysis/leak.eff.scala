object WithFileTest {
  import scala.annotation.internal.local

  class SFile(path: String)

  // sanity check for the pos test

  def length(f: SFile) = f

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  withFile("") { f =>
    length(f) // error
  }

  withFile("") { f =>
    val res = { () => length(f) }
    res() // error
  }
}
