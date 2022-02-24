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

  /** The class used for argument string parsing. E.g. `scala.util.CommandLineParser.FromString`,
   *  but could be something else
   */
  type ArgumentParser[T]

  /** The required result type of the main function */
  type MainResultType

  /** A new command with arguments from `args` */
  def command(args: Array[String], commandName: String, documentation: String, parameterInfoss: MainAnnotation.ParameterInfos*): MainAnnotation.Command[ArgumentParser, MainResultType]
end MainAnnotation

@experimental
object MainAnnotation:

  final class ParameterInfos private (
    /** The name of the parameter */
    val name: String,
    /** The name of the parameter's type */
    val typeName: String,
    /** The docstring of the parameter. Defaults to None. */
    val documentation: Option[String],
    /** The ParameterAnnotations associated with the parameter. Defaults to Seq.empty. */
    val annotations: Seq[ParameterAnnotation],
  ) {
    /** ParameterInfos with a name and the type of the parameter */
    def this(name: String, typeName: String) =
      this(name, typeName, None, Seq.empty)

    /** Copy this ParameterInfos and sets the documentation */
    def withDocumentation(doc: String): ParameterInfos =
      new ParameterInfos(name, typeName, Some(doc), annotations)

    /** Copy this ParameterInfos and sets the annotations */
    def withAnnotations(annots: ParameterAnnotation*): ParameterInfos =
      new ParameterInfos(name, typeName, documentation, annots)

    override def toString: String = s"$name: $typeName"
  }

  /** A class representing a command to run */
  trait Command[ArgumentParser[_], MainResultType]:

    /** The getter for the next argument of type `T` */
    def argGetter[T](name: String, optDefaultGetter: Option[() => T])(using fromString: ArgumentParser[T]): () => T

    /** The getter for a final varargs argument of type `T*` */
    def varargGetter[T](name: String)(using fromString: ArgumentParser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid,
     *  or print usage information and/or error messages.
     */
    def run(program: => MainResultType): Unit
  end Command

  /** Marker trait for annotations that will be included in the ParameterInfos annotations. */
  trait ParameterAnnotation extends StaticAnnotation
end MainAnnotation
