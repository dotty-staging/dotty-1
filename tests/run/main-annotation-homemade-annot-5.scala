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

  override type Parser[T] = util.CommandLineParser.FromString[T]
  override type Result = Any

  def command(info: CommandInfo, args: Array[String]): Command[Parser, Result] =
    new Command[Parser, Result]:
      override def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using p: Parser[T]): () => T = ???

      override def varargGetter[T](using p: Parser[T]): () => Seq[T] = ???

      override def run(program: () => Result): Unit = program()
  end command
