import language.experimental.erasedDefinitions
import annotation.ability
import java.io.IOException

class CanThrow[E] extends Retains[*]
type Top  = Any retains *

def handle[E <: Exception, T <: Top](op: CanThrow[E] ?=> T)(handler: E => T): T =
  val x: CanThrow[E] = ???
  try op(using x)
  catch case ex: E => handler(ex)

def raise[E <: Exception](ex: E)(using CanThrow[E]): Nothing =
  throw ex

def test1: Int =
  def f(a: Boolean): Boolean => CanThrow[IOException] ?=> Int =
    handle {
      if !a then raise(IOException())
      (b: Boolean) => (_: CanThrow[IOException]) ?=>
        if !b then raise(IOException())
        0
    } {
      ex => (b: Boolean) => (_: CanThrow[IOException]) ?=> -1
    }
  handle {
    val g = f(true)
    g(false)
    f(true)(false)
  } {
    ex => -1
  }
