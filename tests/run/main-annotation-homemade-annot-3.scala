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

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  override def command(args: Array[String], commandName: String, docComment: String, parameterInfos: ParameterInfo*) =
    new Command[ArgumentParser, MainResultType]:
      override def argGetter[T](name: String, optDefaultValueGetter: Option[() => T])(using p: ArgumentParser[T]): () => T = ???

      override def varargGetter[T](name: String)(using p: ArgumentParser[T]): () => Seq[T] = ???

      override def run(f: => MainResultType): Unit = f
  end command
