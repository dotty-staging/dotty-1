object Test {
  trait Tag[T]

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
