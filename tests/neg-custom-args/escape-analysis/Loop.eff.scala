object Loop {
  import scala.annotation.internal.{local, entry}

  class Cap()
  def _try[T]()(thunk : (Cap @local => T)) : T = {
    thunk(new Cap())
  }

  class Box[T] {
    var v : T = _
  } 

  @entry def main() : Unit = {
    val i = 0
    val b = new Box[Cap]()

    _try() { cap =>
      while ( i < 10 ) {
        b.v  = cap // make sure we go through a while block.
      }
      b.v // should also error, but due to blames it looks like this error is suppressed
    }
  }
}
