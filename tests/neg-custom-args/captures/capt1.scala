class C
def f(x: C retains *, y: C): () => C =
  () => if x == null then y else y  // error

def g(x: C retains *, y: C): Any =
  () => if x == null then y else y  // error

def h1(x: C retains *, y: C): Any retains x.type =
  def f() = if x == null then y else y
  () => f()  // ok

def h2(x: C retains *): Any =
  def f(y: Int) = if x == null then y else y
  f  // error

class A
type Cap = C retains *
type Top = Any retains *

def h3(x: Cap): A =
  class F(y: Int) extends A:
    def m() = if x == null then y else y
  F(22)  // error

def h4(x: Cap, y: Int): A =
  new A:
    def m() = if x == null then y else y  // error

def foo() =
  val x: C retains * = ???
  def h[X <:Top](a: X)(b: X) = a
  val z2 = h[() => Cap](() => x)(() => C())  // error
  val z3 = h(() => x)(() => C())  // error
