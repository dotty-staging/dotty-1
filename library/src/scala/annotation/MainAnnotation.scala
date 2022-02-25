package scala.annotation

/** MainAnnotation provides the functionality for a compiler-generated main class.
 *  It links a compiler-generated main method (call it compiler-main) to a user
 *  written main method (user-main).
 *  The protocol of calls from compiler-main is as follows:
 *
 *    - create a `command` with the command line arguments,
 *    - for each parameter of user-main, a call to `command.argGetter`,
 *      or `command.varargGetter` if is a final varargs parameter,
 *    - a call to `command.run` with the closure of user-main applied to all arguments.
 *
 *  Example:
 *  ```scala
 *  /** Sum all the numbers
 *   *
 *   *  @param first Fist number to sum
 *   *  @param rest The rest of the numbers to sum
 *   */
 *  @myMain def sum(first: Int, rest: Int*): Int = first + rest.sum
 *  ```
 *  generates
 *  ```scala
 *  object foo {
 *    def main(args: Array[String]): Unit = {
 *      val cmd = new myMain().command(
 *        args = args,
 *        commandName = "sum",
 *        documentation = "Sum all the numbers",
 *        new ParameterInfo("first", "scala.Int", hasDefault=false, isVarargs=false, "Fist number to sum"),
 *        new ParameterInfo("rest", "scala.Int" , hasDefault=false, isVarargs=true, "The rest of the numbers to sum")
 *      )
 *      val args0 = cmd.argGetter[Int](0, None) // using cmd.Parser[Int]
 *      val args1 = cmd.varargGetter[Int] // using cmd.Parser[Int]
 *      cmd.run(() => sum(args0(), args1()*))
 *    }
 *  }
 *  ```
 */
@experimental
trait MainAnnotation extends StaticAnnotation:
  import MainAnnotation.*

  /** The class used for argument string parsing and arguments into a `T` */
  type Parser[T]

  /** The required result type of the main method.
   *
   *  If this type is Any or Unit, any type will be accepted.
   */
  type Result

  /** A new command with arguments from `args`
   *
   *  @param args The command line arguments
   *  @param commandName The name of the command (name of the annotated method)
   *  @param documentation The documentation of the command (without the `@param` documentation)
   *  @param parameterInfos Information about all the parameters (name, type, has default, is varargs and documentation)
   */
  def command(args: Array[String], commandName: String, documentation: String, parameterInfos: ParameterInfo*): Command[Parser, Result]

end MainAnnotation

@experimental
object MainAnnotation:

  /** ParameterInfo with a name, the type of the parameter and if it has a default
   *
   *  @param name The name of the parameter
   *  @param typeName The type of the parameter
   *  @param hasDefault If the parameter has a default argument
   *  @param isVarargs If the parameter is a varargs parameter (can only be true for the last parameter)
   *  @param documentation The documentation of the parameter (from `@param` documentation in the main method)
   */
  final class ParameterInfo (
    paramName: String,
    paramTypeName: String,
    paramHasDefault: Boolean,
    paramIsVarargs: Boolean,
    paramDocumentation: String,
    paramAnnotations: Seq[ParameterAnnotation],
  ) {

    /** The name of the parameter */
    def name: String = paramName

    /** The name of the parameter's type */
    def typeName: String = paramTypeName

    /** If the parameter has a default argument */
    def hasDefault: Boolean = paramHasDefault

    /** If this is a varargs parameter. Can only be true if it is the last parameter. */
    def isVarargs: Boolean = paramIsVarargs

    /** The docstring of the parameter. */
    def documentation: String = paramDocumentation

    /** The ParameterAnnotations associated with the parameter. Defaults to Seq.empty. */
    def annotations: Seq[ParameterAnnotation] = paramAnnotations

    override def toString: String = s"$name: $typeName"
  }

  /** A class representing a command to run */
  trait Command[Parser[_], Result]:

    /** The getter for the `idx`th argument of type `T`
     *
     *   @param idx The index of the argument
     *   @param defaultArgument Optional lambda to instantiate the default argument
     */
    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using Parser[T]): () => T

    /** The getter for a final varargs argument of type `T*` */
    def varargGetter[T](using Parser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid if all arguments are valid
     *
     *  @param program A function containing the call to the main method and instantiation of its arguments
     */
    def run(program: () => Result): Unit
  end Command

  /** Marker trait for annotations that will be included in the ParameterInfo annotations. */
  trait ParameterAnnotation extends StaticAnnotation

end MainAnnotation
