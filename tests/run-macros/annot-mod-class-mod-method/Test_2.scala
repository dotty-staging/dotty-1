@modToString("This is Foo")
class Foo:
  override def toString(): String = "?"

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo", foo)
