package example

trait Foo {
  type T
  type U
  def foo: (T, U)
}

class Bar[A, B] {
  def bar[F <: Foo { type T = A; type U = B }](member: F): (member.T, member.U) = {
    member.foo
  }
}

class Baz {
  def baz[F <: Foo { type T <: String; type B >: List[Nothing] }](member: F): (member.T, member.U) = {
    member.foo
  }
}

trait Methodic {
  def nillary(): Any
}

class Qux[A] {
  def qux[M <: Methodic { def nillary(): A }](m: M): A = m.nillary()
}
