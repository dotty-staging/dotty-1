object Test {

  trait Foo { type T; val x: T }

  implicit intFoo for Foo {
    type T = Int
    val x = 3
  }

  val y: Int = the[Foo].x

}