class Text(val str: String)

given Conversion[String, Text] = Text(_)

@main def Test =

  def f(x: convertibleTo Text, y: => convertibleTo Text, zs: convertibleTo Text*) =
    println(s"${x.str} ${y.str} ${zs.map(_.str).mkString(" ")}")

  f("abc", "def")  // ok
  f("abc", "def", "xyz", "uvw")  // ok
  f("abc", "def", "xyz", Text("uvw"))  // ok

trait A[X]:
  def f(x: X): Unit = ()

trait B[X] extends A[X]:
  override def f(x: X) = super.f(x)

trait C[X] extends A[X]:
  override def f(x: convertibleTo X) = super.f(x)

class D[X] extends B[X], C[X]

def f = new D[Text].f("abc")
