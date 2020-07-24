import scala.tasty.Reflection
import scala.tasty.inspector._

class MyClass {
  def method(args: String*): String = ""
}

object Test {
  def main(args: Array[String]): Unit = {
    println("hello there")
    new TestInspector().inspect("", List("MyClass"))
  }
}

class TestInspector() extends TastyInspector:
  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    import reflect._
    println("hello there")
    inspectClass(reflect)(root)

  private def inspectClass(reflect: Reflection)(tree: reflect.Tree): Unit =
    import reflect.{given _, _}
    tree match {
      case t: reflect.PackageClause =>
        t.stats.map( m => inspectClass(reflect)(m) )

      case t: reflect.ClassDef if !t.name.endsWith("$") =>
        val meths = t.body.collect { case dd: DefDef =>
          dd
        }

        meths.foreach { dd =>
          dd.paramss.head.foreach { vd =>
            println(vd.show)
          }
        }

      case x =>
    }
