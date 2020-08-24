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
  object hack {
    import scala.tasty.Reflection

    import dotty.tools.dotc.Compiler
    import dotty.tools.dotc.Driver
    import dotty.tools.dotc.Run
    import dotty.tools.dotc.core.Contexts.Context
    import dotty.tools.dotc.core.Mode
    import dotty.tools.dotc.core.Phases.Phase
    import dotty.tools.dotc.fromtasty._
    import dotty.tools.dotc.tastyreflect.ReflectionImpl
    import dotty.tools.dotc.util.ClasspathFromClassloader

    import java.io.File.pathSeparator

    trait TastyInspector:
      self =>

      /** Process a TASTy file using TASTy reflect */
      protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit

      /** Load and process TASTy files using TASTy reflect
      *
      *  @param classpath Classpath where the classes are located
      *  @param classes classes to be inspected
      */
      def inspect(classpath: String, classes: List[String]): Unit =
        if (classes.isEmpty)
          throw new IllegalArgumentException("Parameter classes should no be empty")

        class InspectorDriver extends Driver:
          override protected def newCompiler(implicit ctx: Context): Compiler = new TastyFromClass

        class TastyFromClass extends TASTYCompiler:

          override protected def frontendPhases: List[List[Phase]] =
            List(new ReadTasty) :: // Load classes from tasty
            Nil

          override protected def picklerPhases: List[List[Phase]] = Nil

          override protected def transformPhases: List[List[Phase]] = Nil

          override protected def backendPhases: List[List[Phase]] =
            List(new TastyInspectorPhase) ::  // Print all loaded classes
            Nil

          override def newRun(implicit ctx: Context): Run =
            reset()
            new TASTYRun(this, ctx.fresh.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))

        end TastyFromClass

        class TastyInspectorPhase extends Phase:

          override def phaseName: String = "tastyInspector"

          override def run(implicit ctx: Context): Unit =
            // CHANGE
            val reflect = ReflectionImpl(ctx.fresh.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))
            // /CHANGE
            self.processCompilationUnit(reflect)(ctx.compilationUnit.tpdTree.asInstanceOf[reflect.Tree])

        end TastyInspectorPhase

        val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
        val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$pathSeparator$currentClasspath" :: classes
        (new InspectorDriver).process(args.toArray)
      end inspect


    end TastyInspector
  }

  def main(args: Array[String]): Unit = {
    new TestInspector().inspect("", List("tests.A"))
  }
}

class TestInspector() extends Test.hack.TastyInspector:

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    import reflect._
    inspectClass(reflect)(root)

  private def inspectClass(reflect: Reflection)(tree: reflect.Tree): Unit =
    import reflect.{given _, _}
    tree match {
      case t: reflect.PackageClause =>
        t.symbol.tree match {
          case d: reflect.PackageDef =>
            println(s"${t.symbol.show}[${t.symbol.flags.show}] == packagedef")
            assert(t.symbol.flags.is(Flags.JavaDefined))
            assert(d.members.isEmpty)
        }
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
          // s.tree
        }
        t.stats.map( m => inspectClass(reflect)(m) )
      case x =>
    }
