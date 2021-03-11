object Test {
  trait Tag[T]

  // works: Int = Z = Y = T
  def func1[Z >: Int <: Int, Y >: Z <: Z, T >: Y <: Y]: T = {
    0
  }

  // works: Int = Z = Y = T
  def func2[
    Z >: Int <: Int,
    Y >: Z <: Z,
    T
  ]: Tag[T] => T = {
    case _ : Tag[Y] => 0
  }

  // works: Int = Z = Y = X = T
  def func3[Z >: Int <: Int, Y >: Z <: Z, X >: Y <: Y, T >: X <: X]: T = {
    0
  }

  // expect: Int = Z = Y = X = T
  // actual: can not derive T = Int
  def func4[
    Z >: Int <: Int,
    Y >: Z <: Z,
    X >: Y <: Y,
    T]: Tag[T] => T = {
    case _ : Tag[X] => 0  // error
  }
}
