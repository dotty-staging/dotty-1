import scala.annotation.*

@mainNoArgs def foo() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    val clazz = Class.forName("foo")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, Array[String]())
end Test

@experimental
class mainNoArgs extends MainAnnotation:
  import MainAnnotation.*

  type Parser[T] = util.CommandLineParser.FromString[T]

  def command(info: CommandInfo, args: Array[String]): Command = new Command
  @experimental
  class Command:
    def argGetter[T](idx: Int, defaultArgument: Option[() => T]): () => T = ???
    def varargGetter[T]: () => Seq[T] = ???
    def run(program: () => Any): Unit = program()
