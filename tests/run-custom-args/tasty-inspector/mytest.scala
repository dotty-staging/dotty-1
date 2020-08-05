import scala.tasty.Reflection
import scala.tasty.inspector._

package tests {

class A {
  /** This is a method. */
  def method(s: String): String = s

  class AA

  object AA
}

/** Companion object to test linking */
object A

class B extends A {
  /** This is a method. */
  def otherMethod(s: String): String = s

  class BB
}

object B {
  type Z = Int
  val Z: Int = 0
}

class C 
class D[T]
class E[T] extends D[T]

class Constructors(a: String):
    def this() = this("Ala")
    def this(a: A)(b: A) = this("Ala")

/** Some methods to tests */
class Methods:
 def nobraces: A = ???
 /** Class doc test.
   * @throws [[Error]] Throws errors.
   */
 def simple(): B = ???
 def oneParam(a: A): B = ???
 def multipleParams(a: A, b: B): C = ???
 def vararg(a: A*): C = ???
 def multipleList(a: A)(b: B): C = ???

 def generic[T](a: D[T]): D[T] = ???
 def generic2[T, V](a: D[T], b: E[V]): D[T] = ???

 def primitives(a: Int, b: Double, c: Short): Byte = 0
 def strings(a: String): String = ""
 def arrays(a: Array[String], b: Array[Int]): Array[Double] = ???
}

object Test {
  def main(args: Array[String]): Unit = {
    new TestInspector().inspect("", List("tests.A"))
  }
}

class TestInspector() extends TastyInspector:

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    import reflect._
    inspectClass(reflect)(root)

  private def inspectClass(reflect: Reflection)(tree: reflect.Tree): Unit =
    import reflect.{given _, _}
    tree match {
      case t: reflect.PackageClause =>
        def hackMembersOf(using r: Reflection)(rsym: r.Symbol) = {
          import dotty.tools.dotc
          given dotc.core.Contexts.Context = r.rootContext.asInstanceOf
          val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
          val members = sym.info.decls.iterator
          // println(s"members of ${sym.show} : ${members.map(_.show).mkString(", ")}")
          members.asInstanceOf[Iterator[r.Symbol]]
        }

        hackMembersOf(using reflect)(t.symbol).foreach { s =>
          println(s">>> ${s.show}")
          println(s">>> ${s.pos}")
          println(s">>> [${s.flags.show}]")
        }
        t.stats.map( m => inspectClass(reflect)(m) )
      case x =>
    }
