object ActorMutPoolTest {
  import scala.annotation.internal.local

  class CanThrow
  class Actor(ct: CanThrow)

  class MutPair {
    var _1: Actor = _
    var _2: Actor = _
  }

  type Pool = MutPair
  def foo(@local ct: CanThrow) = {
    // val pool = new Array[Actor](2)
    // pool(0) = new Actor(ct)
    // pool(1) = new Actor(ct)
    val pool = new MutPair
    pool._1 = new Actor(ct)
    pool._2 = new Actor(ct)
    // pool._1
    bar(pool)
    // pool
  }

  def bar(@local pool: Pool) = {
    ()
  }
}
