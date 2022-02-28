import scala.annotation.*

@mainManyArgs(Some(1)) def foo() = println("Hello world!")
@mainManyArgs(None) def bar() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    for (methodName <- List("foo", "bar"))
      val clazz = Class.forName(methodName)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array[String]())
end Test

@experimental
class mainManyArgs(o: Option[Int]) extends MainAnnotation:
  import MainAnnotation.*

  def command(info: CommandInfo, args: Array[String]): Command = new Command

  @experimental
  class Command:
    def argGetter[T](idx: Int, defaultArgument: Option[() => T]): () => T = ???
    def varargGetter[T]: () => Seq[T] = ???
    def run(program: () => Any): Unit = program()
