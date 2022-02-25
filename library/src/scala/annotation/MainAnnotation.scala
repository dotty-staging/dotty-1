package scala.annotation

/** MainAnnotation provides the functionality for a compiler-generated main class.
 *  It links a compiler-generated main method (call it compiler-main) to a user
 *  written main method (user-main).
 *  The protocol of calls from compiler-main is as follows:
 *
 *    - create a `command` with the command line arguments,
 *    - for each parameter of user-main, a call to `command.argGetter`,
 *      or `command.argsGetter` if is a final varargs parameter,
 *    - a call to `command.run` with the closure of user-main applied to all arguments.
 */
@experimental
trait MainAnnotation extends StaticAnnotation:
  import MainAnnotation.*

  /** The class used for argument string parsing. E.g. `scala.util.CommandLineParser.FromString`,
   *  but could be something else
   */
  type Parser[T]

  /** The required result type of the main function */
  type Result

  /** A new command with arguments from `args` */
  def command(args: Array[String], commandName: String, documentation: String, parameterInfos: ParameterInfo*): Command[Parser, Result]

end MainAnnotation

@experimental
object MainAnnotation:

  final class ParameterInfo private (
    paramName: String,
    paramTypeName: String,
    paramHasDefault: Boolean,
    paramIsVarargs: Boolean,
    paramDocumentation: String,
    paramAnnotations: Seq[ParameterAnnotation],
  ) {

    /** ParameterInfo with a name, the type of the parameter and if it has a default */
    def this(name: String, typeName: String, hasDefault: Boolean, isVarargs: Boolean, documentation: String) =
      this(name, typeName, hasDefault, isVarargs, documentation, Seq.empty)

    /** The name of the parameter */
    def name: String = paramName

    /** The name of the parameter's type */
    def typeName: String = paramTypeName

    /** If the parameter has a default value */
    def hasDefault: Boolean = paramHasDefault

    /** If this is a varargs parameter. Can only be true if it is the last parameter. */
    def isVarargs: Boolean = paramIsVarargs

    /** The docstring of the parameter. */
    def documentation: String = paramDocumentation

    /** The ParameterAnnotations associated with the parameter. Defaults to Seq.empty. */
    def annotations: Seq[ParameterAnnotation] = paramAnnotations

    /** Copy this ParameterInfo and sets the annotations */
    def withAnnotations(annotations: ParameterAnnotation*): ParameterInfo =
      new ParameterInfo(paramName, typeName, paramHasDefault, paramIsVarargs, documentation, annotations)

    override def toString: String = s"$name: $typeName"
  }

  /** A class representing a command to run */
  trait Command[Parser[_], Result]:

    /** The getter for the `idx`th argument of type `T` */
    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using Parser[T]): () => T

    /** The getter for a final varargs argument of type `T*` */
    def varargGetter[T](using Parser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid,
     *  or print usage information and/or error messages.
     */
    def run(program: => Result): Unit
  end Command

  /** Marker trait for annotations that will be included in the ParameterInfo annotations. */
  trait ParameterAnnotation extends StaticAnnotation

end MainAnnotation
