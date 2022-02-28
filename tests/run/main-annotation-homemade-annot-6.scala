import scala.annotation.*

/** Foo docs */
@myMain def foo(i: Int, j: String = "2") = println(s"foo($i, $j)")
/** Bar docs
 *
 *  @param i the first parameter
 */
@myMain def bar(@MyParamAnnot(3) i: List[Int], rest: Int*) = println(s"bar($i, ${rest.mkString(", ")})")

object Test:
  def main(args: Array[String]) =
    for (methodName <- List("foo", "bar"))
      val clazz = Class.forName(methodName)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array[String]())
end Test

@experimental
class myMain extends MainAnnotation:
  import MainAnnotation.*

  override type Parser[T] = Make[T]

  def command(info: CommandInfo, args: Array[String]): Command =
    def paramInfoString(paramInfo: ParameterInfo) =
      import paramInfo.*
      s"    ParameterInfo(name=\"$name\", typeName=\"$typeName\", hasDefault=$hasDefault, isVarargs=$isVarargs, documentation=\"$documentation\", annotations=$annotations)"
    println(
      s"""command(
         |  ${args.mkString("Array(", ", ", ")")},
         |  ${info.name},
         |  "${info.documentation}",
         |  ${info.parameters.map(paramInfoString).mkString("Seq(\n", ",\n", "\n  )*")}
         |)""".stripMargin)
    new Command

  @experimental
  class Command:

    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using p: Parser[T]): () => T =
      println(s"argGetter($idx, ${defaultArgument.map(_())})")
      () => p.make

    def varargGetter[T](using p: Parser[T]): () => Seq[T] =
      println("varargGetter()")
      () => Seq(p.make, p.make)

    def run(f: () => Any): Unit =
      println("run()")
      f()
      println()
  end Command

@experimental
case class MyParamAnnot(n: Int) extends MainAnnotation.ParameterAnnotation

trait Make[T]:
  def make: T

given Make[Int] with
  def make: Int = 42


given Make[String] with
  def make: String = "abc"

given [T: Make]: Make[List[T]] with
  def make: List[T] = List(summon[Make[T]].make)
