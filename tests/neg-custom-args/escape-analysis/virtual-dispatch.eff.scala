object test {
  import annotation.internal.local

  object case_straight {
    def foo(@local c: lib.Console): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      v.value = lib.StrictBox(0)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value
    }

    def bar(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      v.value = lib.StrictBox(0)
      v.value = lib.LazyBox(() => 0)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }

    def bar_crazy(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      v.value = lib.StrictBox(0)
      def vv = {
        v.value = lib.LazyBox(() => 0)
        v
      }
      vv.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }

    def baz(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      v.value = lib.StrictBox(0)
      val res = v.value.map { i =>
        c.println(i)
        i
      }
      v.value = lib.LazyBox(() => 0)
      res
    }

    // interestingly, this succeeds
    def baz_alt(@local c: lib.Console): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      v.value = lib.StrictBox(0)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value = lib.LazyBox(() => 0)
      v.value
    }
  }

  object case_method {
    def foo(@local c: lib.Console): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      foo_set(v)
      v.value
    }

    def foo_set(v: lib.Cell[lib.Box[Int]]): Unit = {
      v.value = lib.StrictBox(0)
    }

    def bar(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      bar_set(v)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }

    def bar_set(v: lib.Cell[lib.Box[Int]]): Unit = {
      v.value = lib.StrictBox(0)
      v.value = lib.LazyBox(() => 0)
    }
  }

  object case_method_alt {
    def foo(@local c: lib.Console): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      foo_impl(v)(c)
      v.value
    }

    def foo_impl(v: lib.Cell[lib.Box[Int]])(c: lib.Console): Unit = {
      v.value = lib.StrictBox(0)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
    }

    def bar(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      bar_impl(v)(c)
      v.value
    }

    def bar_impl(v: lib.Cell[lib.Box[Int]])(c: lib.Console): Unit = {
      v.value = lib.StrictBox(0)
      v.value = lib.LazyBox(() => 0)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
    }
  }

  object case_local {
    def foo(
      @local c: lib.Console
    ): lib.Box[Int] = {
      def foo_set(v: lib.Cell[lib.Box[Int]]): Unit = {
        v.value = lib.StrictBox(0)
      }

      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      foo_set(v)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value
    }

    def bar(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      def bar_set(v: lib.Cell[lib.Box[Int]]): Unit = {
        v.value = lib.StrictBox(0)
        v.value = lib.LazyBox(() => 0)
      }

      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      bar_set(v)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }
  }

  object case_lambda {
    def foo(
      @local c: lib.Console
    ): lib.Box[Int] = {
      val foo_set: (v: lib.Cell[lib.Box[Int]]) => Unit =
        (v: lib.Cell[lib.Box[Int]]) => {
          v.value = lib.StrictBox(0)
        }

      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      foo_set(v)
      v.value = v.value.map { i =>
        c.println(i)
        i
      }
      v.value
    }

    def bar(
      @local c: lib.Console // error
    ): lib.Box[Int] = {
      val bar_set: (v: lib.Cell[lib.Box[Int]]) => Unit =
        (v: lib.Cell[lib.Box[Int]]) => {
          v.value = lib.StrictBox(0)
          v.value = lib.LazyBox(() => 0)
        }

      val v: lib.Cell[lib.Box[Int]] = lib.Cell()
      bar_set(v)
      v.value = v.value.map { i => // error
        c.println(i)
        i
      }
      v.value
    }
  }

  object lib {
    trait Console {
      def println(a: Any): Unit
    }

    class Cell[T] {
      var value: T = _
    }

    trait Box[T] {
      def map[U](f: T => U): Box[U]
    }

    class StrictBox[T](t: T) extends Box[T] {
      def map[U](f: T => U): StrictBox[U] =
        StrictBox(f(t))
    }

    class LazyBox[T](val thunk: () => T) extends Box[T] {
      def map[U](f: T => U): LazyBox[U] =
        LazyBox(() => f(thunk()))
    }
  }
}
