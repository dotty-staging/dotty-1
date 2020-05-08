// Tests if annotations nest properly.

object Nested {
  import scala.annotation.internal.{local, entry}

  class Cap

  def _try[T]()(thunk : (Cap @local => T)) : T = {
    thunk(new Cap)
  }

  @entry def main() : Unit = {
    _try() { c1 =>
      _try() { c2 => 
        c1 // should not fail, as c1 is still good outside of this "_try"
           // block.
      }
      ()
    }
    ()
  }
  
}
