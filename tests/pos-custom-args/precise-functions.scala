import scala.compiletime.ops.int._



object PreciseFunctions {
  dependent case class Foo(v: Int)
  class Bar(val w: Int, a: Int) extends Foo(a):
    private[PreciseFunctions] val b = a

  /*
  dependent def id(a: Int) = a

  val v1: 42 = id(42)
  val v2: v1.type = id(v1)
  val v3: Int = 43

  dependent val v4: v1.type + v3.type = v1 + v3
  */
  val foo = new Foo(42)
  val bar = new Bar(43, 44)

}
