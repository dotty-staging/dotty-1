object WithFileTest {
  import scala.annotation.internal.local

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  withFile("") { f =>
    f // error
  }

  withFile("") { f1 =>
    val res: () => SFile = withFile("") { f2 =>
      { () => f1 }
    }
    res() // error
  }

  withFile("") { f =>
    val res = { () => f }
    res // error
  }

  withFile("") { f =>
    {
      val res = { () => f }
      res // error
    }
  }

  class Container[T](value: T)

  withFile("") { f =>
    new Container(f) // error
  }

  class Container2[T](val value: T)

  withFile("") { f =>
    new Container2(f).value // error
  }

}
