 object Test {
  def baz(x: Object) = {
    val s @ (_s: String) = x: @unchecked
    x
  }
  def main(args: Array[String]): Unit = {
    assert(baz("1") == "1")
  }
}
