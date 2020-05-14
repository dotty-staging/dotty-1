object RecursionWithDifferentShapes {
  import scala.annotation.internal.{local, entry}

  case class SPair[T](a : T, b: T)
  class SItem

  def makePair(item: SItem, level: Int) : SPair[SItem] = {
    if (level == 0) {
      new SPair(item, null)
    } else {
      new SPair(null, makePair(item, 0).a)
    }
  }

  def cantEscape[T](thunk : (SItem @local => T)) : T = {
    thunk(new SItem)
  }

  def willEscape(item: SItem) : SItem = {
    makePair(item, 1).b
  }

  // this is a strange place for the error to be blamed...
  @entry def main() = { // error
    val result = cantEscape(willEscape)
    ()
  }
}
