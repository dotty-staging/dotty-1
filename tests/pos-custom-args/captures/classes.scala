class B
type Cap = {*} B
class C(val n: Cap):
  this: ({n} C) =>
  def foo(): {n} B = n


def test(x: Cap, y: Cap) =
  val c0 = C(x)
  val c1: {x} C {val n: {x} B} = c0
  val z = c1.foo()
  z: ({x} B)

  val c2 = if ??? then C(x) else identity(C(y))
  val c3: {x, y} C { val n: {x, y} B } = c2
  val z1 = c3.foo()
  z1: B @retains(x, y)
