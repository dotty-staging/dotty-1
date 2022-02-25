import scala.annotation.*
import collection.mutable

/** Sum all the numbers
 *
 *  @param first Fist number to sum
 *  @param rest The rest of the numbers to sum
 */
@myMain def sum(first: Int, rest: Int*): Int = first + rest.sum


object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("sum")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("23", "2", "3"))
end Test

@experimental
class myMain extends MainAnnotation:
  import MainAnnotation.{ Command, CommandInfo, ParameterInfo }

  // Parser used to parse command line arguments
  type Parser[T] = util.CommandLineParser.FromString[T]

  // Result type of the annotated method
  type Result = Int

  /** A new command with arguments from `args` */
  def command(info: CommandInfo, args: Array[String]): Command[Parser, Result] =
    if args.contains("--help") then
      println(info.documentation)
      System.exit(0)
    assert(info.parameters.forall(!_.hasDefault), "Default arguments are not supported")
    val (plainArgs, varargs) =
      if info.parameters.last.isVarargs then
        val numPlainArgs = info.parameters.length - 1
        assert(numPlainArgs <= args.length, "Not enough arguments")
        (args.take(numPlainArgs), args.drop(numPlainArgs))
      else
        assert(info.parameters.length <= args.length, "Not enough arguments")
        assert(info.parameters.length >= args.length, "Too many arguments")
        (args, Array.empty[String])
    new MyCommand(plainArgs, varargs)

  @experimental
  class MyCommand(plainArgs: Seq[String], varargs: Seq[String]) extends Command[util.CommandLineParser.FromString, Int]:

    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using parser: Parser[T]): () => T =
      () => parser.fromString(plainArgs(idx))

    def varargGetter[T](using parser: Parser[T]): () => Seq[T] =
      () => varargs.map(arg => parser.fromString(arg))

    def run(program: () => Result): Unit =
      println("executing program")
      val result = program()
      println("result: " + result)
      println("executed program")
  end MyCommand
end myMain
