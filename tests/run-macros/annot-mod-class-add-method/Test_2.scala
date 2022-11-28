@genToString("This is Foo")
class Foo

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo", foo)
