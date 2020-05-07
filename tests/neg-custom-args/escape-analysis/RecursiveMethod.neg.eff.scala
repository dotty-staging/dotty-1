object RecursiveMethod {
  import scala.annotation.internal.{local, entry}

  class SFile

  class Class(f: SFile) {
    def count(i: Int): SFile =
      if (i == 0) f else count(i - 1)
  }

  def withFile[T](thunk: (SFile @local) => T): T =
    thunk(new SFile)

  @entry def main: Unit = { // error
    withFile {
      f =>
      val v = new Class(f)

      v.count(5)
    }
  }
}
