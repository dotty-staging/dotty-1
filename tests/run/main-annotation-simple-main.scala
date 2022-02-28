import scala.concurrent._
import scala.annotation.*
import scala.collection.mutable
import ExecutionContext.Implicits.global
import duration._

@simpleMain def myMain(args: String*): Unit =
  println("args: " + args)

object Test:
  def callMain(cls: String, args: Array[String]): Unit =
    val clazz = Class.forName(cls)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain("myMain", Array("1"))
end Test

@experimental
class simpleMain extends MainAnnotation:
  import MainAnnotation.CommandInfo

  // This is a toy example, it only works with positional args
  def command(info: CommandInfo, args: Array[String]): Command = new Command

  class Command:
    @compileTimeOnly("@simpleMain must have String* arguments")
    def argGetter[T](idx: Int, defaultArgument: Option[() => T]): () => String = ???

    def varargGetter[T]: () => Seq[String] = () => Seq()

    def run(f: () => Unit): Unit = f()
end simpleMain
