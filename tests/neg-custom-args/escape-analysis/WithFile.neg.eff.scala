object WithFileTest {
  import scala.annotation.internal.local

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f) // error // error // error // error // error // error
  }

  def foo_a(@local u: Unit): Unit = {
    withFile("") { f =>
      f
    }
  }

  def foo_b(@local u: Unit): Unit = {
    withFile("") { f1 =>
      val res: () => SFile = withFile("") { f2 =>
        { () => f1 }
      }
      res()
    }
  }

  def foo_c(@local u: Unit): Unit = {
    withFile("") { f =>
      val res = { () => f }
      res
    }
  }

  def foo_d(@local u: Unit): Unit = {
    withFile("") { f =>
      {
        val res = { () => f }
        res
      }
    }
  }

  class Container[T](value: T)
  def bar(@local u: Unit): Unit = {
    withFile("") {
      f =>
      new Container(f)
    }
  }

  class Container2[T](val value: T)
  def baz(@local u: Unit): Unit = {
    withFile("") { f =>
      new Container2(f).value
    }
  }

}
