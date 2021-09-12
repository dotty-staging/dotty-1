abstract class LIST[+T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]
class CONS[+T](x: T, xs: LIST[T]) extends LIST[T]:
  def isEmpty = false
  def head = x
  def tail = xs
object NIL extends LIST[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

class C
type Cap = {*} C

def test(c: Cap, d: Cap) =
  def f(x: Cap): Unit = if c == x then ()
  def g(x: Cap): Unit = if d == x then ()
  val y = f
  val ys = CONS(y, NIL)
  val zs =
    val z = g
    CONS(z, ys)
  val z1 = zs.head
  val z1c: {y, d} Cap => Unit = z1
  val ys1 = zs.tail
  val y1 = ys1.head



