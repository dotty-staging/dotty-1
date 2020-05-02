object WithFileTest {
  import scala.annotation.internal.{local, entry}

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  @entry def foo_a(): Unit = { // error
    withFile("") { f =>
      f
    }
  }

  @entry def foo_b(): Unit = { // error
    withFile("") { f1 =>
      val res: () => SFile = withFile("") { f2 =>
        { () => f1 }
      }
      res()
    }
  }

  @entry def foo_c(): Unit = { // error
    withFile("") { f =>
      val res = { () => f }
      res
    }
  }

  @entry def foo_d(): Unit = { // error
    withFile("") { f =>
      {
        val res = { () => f }
        res
      }
    }
  }

  class Container[T](value: T)
  @entry def bar(): Unit = { // error
    withFile("") {
      f =>
      new Container(f)
    }
  }

  class Container2[T](val value: T)
  @entry def baz(): Unit = { // error
    withFile("") { f =>
      new Container2(f).value
    }
  }

}
