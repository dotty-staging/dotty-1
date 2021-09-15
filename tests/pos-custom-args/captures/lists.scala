abstract class LIST[+T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]
  def map[U](f: {*} T => U): LIST[U] =
    if isEmpty then NIL
    else CONS(f(head), tail.map(f))

class CONS[+T](x: T, xs: LIST[T]) extends LIST[T]:
  def isEmpty = false
  def head = x
  def tail = xs
object NIL extends LIST[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

def map[A, B](f: {*} A => B)(xs: LIST[A]): LIST[B] =
  xs.map(f)

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


  def m1[A, B] =
    (f: {*} A => B) => (xs: LIST[A]) => xs.map(f)

  def m1c: (f: {*} String => Int) => {f} LIST[String] => LIST[Int] = m1[String, Int]

  def m2 = [A, B] =>
      (f: {*} A => B) => (xs: LIST[A]) => xs.map(f)

  def m2c: [A, B] => (f: {*} A => B) => {f} LIST[A] => LIST[B] = m2

  val a0 = identity[{d, y} Cap => Unit]
  val a1 = zs.map[{d, y} Cap => Unit](a0)
//  val a2 = zs.map[{d, y} Cap => Unit](identity[{d, y} Cap => Unit])
//  val a3 = zs.map(identity[{d, y} Cap => Unit])
//  val a4 = zs.map(identity)
//  val a2 = map(identity)(zs)
//  val a3 = m1(identity)(zs)

