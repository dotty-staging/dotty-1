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

  override type Parser[T] = util.CommandLineParser.FromString[T]
  override type Result = Any

  override def command(args: Array[String], commandName: String, docComment: String, parameterInfos: ParameterInfo*) =
    new Command[Parser, Result]:
      override def parseArg[T](idx: Int, optDefaultGetter: Option[() => T])(using p: Parser[T]): Option[T] = None

      override def parseVararg[T](using p: Parser[T]): Option[Seq[T]] = None

      override def run(f: => Result): Unit = f
  end command
