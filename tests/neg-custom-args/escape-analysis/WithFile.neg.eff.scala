object WithFileTest {
  import scala.annotation.internal.local

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  def foo(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      f
    }

    withFile("") {
      f1 => // error
      val res: () => SFile = withFile("") { f2 =>
        { () => f1 }
      }
      res()
    }

    withFile("") {
      f => // error
      val res = { () => f }
      res
    }

    withFile("") {
      f => // error
      {
        val res = { () => f }
        res
      }
    }
  }

  class Container[T](value: T)
  def bar(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      new Container(f)
    }
  }

  class Container2[T](val value: T)
  def baz(@local u: Unit): Unit = {
    withFile("") {
      f => // error
      new Container2(f).value
    }
  }

}
