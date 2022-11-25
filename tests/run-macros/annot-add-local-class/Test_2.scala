@main def Test(): Unit =
  @addClass def foo(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head))
  // class Test extends Object {
  //   def run() =
  //     println("macro generated main")
  //     println("executed in: " + getClass.getName)
  // }
  // def foo(): Unit =
  //   new Test().run

  foo()
