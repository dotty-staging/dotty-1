object TestMain {
  def main(args: Array[String]): Unit = {
    testExtensionProperty()
  }

  case class Foo(var foo: String)

  object fooExt:
    extension (thisFoo: Foo)
      // define `Foo`-extension property with getter and setter delegating to `Foo.foo`
      def fooExt: String = thisFoo.foo
      def fooExt_= (value: String): Unit = { thisFoo.foo = value }

  def testExtensionProperty(): Unit = {
    import fooExt._
    val foo = Foo("initVal")
    assert(foo.fooExt == "initVal")
    foo.fooExt = "updatedVal"
    assert(foo.foo == "updatedVal")
    assert(foo.fooExt == "updatedVal")
  }
}