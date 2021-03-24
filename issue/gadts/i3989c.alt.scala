import scala.Option
object Test extends App {
  trait A[+X]
  class B[+X](val x: X) extends A[X]
  object B {
    def unapply[X](b: B[X]): Option[X] = Some(b.x)
  }

  class C[+X](x: Any) extends B[Any](x) with A[X]
  def f(a: A[Int]): Int =
    if a.isInstanceOf[B[Int]] then
      a.asInstanceOf[B[Int]].x
    else
      0
  f(new C[Int]("foo"))
}
