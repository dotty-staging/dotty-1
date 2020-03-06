object NastyLambda {
  import scala.annotation.internal.local

  def foo(
    @local i: Int // error
  ): Int = {
    bar(() => i)
  }

  def bar(@local f: () => Int): Int = {
    f()
  }
}
