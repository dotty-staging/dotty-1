object ActorMutPoolTest {
  import scala.annotation.internal.local

  class CanThrow
  class Actor(ct: CanThrow)

  class MutPair {
    var _1: Actor = _
    var _2: Actor = _
  }

  type Pool = MutPair
  def foo(@local ct: CanThrow): Unit = {
    val pool = new MutPair
    pool._1 = new Actor(ct) // error
    pool._2 = new Actor(ct) // error
  }
}
