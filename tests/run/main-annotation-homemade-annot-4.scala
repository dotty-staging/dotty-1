import scala.annotation.*

@mainManyArgs(1, "B", 3) def foo() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    val clazz = Class.forName("foo")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, Array[String]())
end Test

@experimental
class mainManyArgs(i1: Int, s2: String, i3: Int) extends MainAnnotation:
  import MainAnnotation.*

  type Parser[T] = util.CommandLineParser.FromString[T]

  def command(info: CommandInfo, args: Array[String]): Command = new Command

  @experimental
  class Command:
    def argGetter[T](idx: Int, optDefaultGetter: Option[() => T]): () => T = ???
    def varargGetter[T]: () => Seq[T] = ???
    def run(program: () => Any): Unit = program()

