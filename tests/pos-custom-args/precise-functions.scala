import scala.compiletime.ops.int._

dependent case class Foo(v: Int)
class Bar(val w: Int, a: Int) extends Foo(a)

dependent class Vec(val size: Int):
  def add(v2: Vec(size.type)) = new Vec(size)
  dependent def concat(v2: Vec) = new Vec(size + v2.size)

object Test {
  dependent def id(a: Int) = a

  val v1: 42 = id(42)
  val v2: v1.type = id(v1)
  val v3: Int = 43

  val foo = Foo(42)
  val foo2 = Foo(v1)
  val foo3: Foo(v3.type) = Foo(v3)
  val bar = Bar(43, 44)

  def test() =
    val x: Int = 1
    val y = 2 + x

  dependent def testPrecise() =
    val x: Int = 1
    val y = 2 + x

  val v5 = Vec(2)
  val v6 = v5.add(new Vec(2)).concat(new Vec(3))
}
