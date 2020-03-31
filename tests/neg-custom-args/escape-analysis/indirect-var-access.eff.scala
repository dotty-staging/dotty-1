object WithFileTest {
  import scala.annotation.internal.local

  final class Cell[T]() {
    var value: T = _
    def getValue = value
  }

  def foo(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      val c = Cell[Fun[SFile]]()
      c.value = new Id[SFile]
      c.getValue.apply(f)
    }
  }

  final class Cell2[T]() {
    var value: T = _
    def getValue = this.value
  }

  def bar(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      val c = Cell[Fun[SFile]]()
      c.value = new Id[SFile]
      c.getValue.apply(f)
    }
  }

  trait Fun[T] { def apply(t: T): T }
  class Id[T] extends Fun[T] { def apply(t: T): T = t }

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

}
