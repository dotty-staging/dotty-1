object NastyLambdaStrikesBack {
  import scala.annotation.internal.local

  class Cell {
    var i: Int = 0
  }

  def foo(
    @local i: Int
  ): Cell = {
    val c: Cell = Cell()
    bar(c, () => i)
  }

  def bar(c: Cell, @local f: () => Int): Cell = {
    c.i = f() // error
    c
  }
}
