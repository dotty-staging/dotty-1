object MatchTest {
  import scala.annotation.internal.local

  def foo(opt: Option[Int], @local x: Int): Int = {
    opt match {
      case Some(i) => i
      case None => 0
    }
  }

  def bar(
    opt: Option[Int],
    @local x: Int // error
  ): Int = {
    opt match {
      case Some(i) =>
        x
      case None => 0
    }
  }

  def baz(
    opt: Option[Int],
    @local x: Int // error
  ): Int = {
    opt match {
      case Some(1) =>
        x
      case Some(_) | None =>
        0
    }
  }

}
