object WithFileTest {
  import scala.annotation.internal.{local, entry}

  class SFile(path: String)

  def length(f: SFile): Int = 0

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  @entry def main(): Unit = {
    withFile("") { f =>
      length(f)
    }

    withFile("") { f =>
      val res: () => Long = { () => length(f) }
      res()
    }
  }
}
