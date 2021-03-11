object Test {
  trait Tag[T]

  // works: Int = Z = Y = T
  def func2[
    Z >: Int <: Int,
    Y >: Z <: Z,
    T
  ]: Tag[T] => T = {
    case _ : Tag[Y] => 0
  }
}
