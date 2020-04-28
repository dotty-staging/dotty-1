object WithFileTest {
  import scala.annotation.internal.{local, entry}

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f) // error // error // error // error // error // error
  }

  @entry def foo_a(): Unit = {
    withFile("") { f =>
      f
    }
  }

  @entry def foo_b(): Unit = {
    withFile("") { f1 =>
      val res: () => SFile = withFile("") { f2 =>
        { () => f1 }
      }
      res()
    }
  }

  @entry def foo_c(): Unit = {
    withFile("") { f =>
      val res = { () => f }
      res
    }
  }

  @entry def foo_d(): Unit = {
    withFile("") { f =>
      {
        val res = { () => f }
        res
      }
    }
  }

  class Container[T](value: T)
  @entry def bar(): Unit = {
    withFile("") {
      f =>
      new Container(f)
    }
  }

  class Container2[T](val value: T)
  @entry def baz(): Unit = {
    withFile("") { f =>
      new Container2(f).value
    }
  }

}
