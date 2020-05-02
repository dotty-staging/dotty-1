object ActorMutPoolTest {
  import scala.annotation.internal.{local, entry}

  class CanThrow
  class Actor(ct: CanThrow)

  class MutPair {
    var _1: Actor = _
    var _2: Actor = _
  }

  def local[T, U](t: T)(thunk: (T @local) => U): U =
    thunk(t)

  type Pool = MutPair
  @entry def foo(): Unit =
    local (new CanThrow) { ct =>
      val pool = new MutPair
      pool._1 = new Actor(ct) // error
      pool._2 = new Actor(ct) // error
    }
}
