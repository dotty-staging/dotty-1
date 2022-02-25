import scala.annotation.*
import collection.mutable

@newMain def happyBirthday(age: Int, name: String, others: String*) =
  val suffix =
    age % 100 match
    case 11 | 12 | 13 => "th"
    case _ =>
      age % 10 match
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case _ => "th"
  val bldr = new StringBuilder(s"Happy $age$suffix birthday, $name")
  for other <- others do bldr.append(" and ").append(other)
  println(bldr)


object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("happyBirthday")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("23", "Lisa", "Peter"))
end Test



@experimental
final class newMain extends MainAnnotation:
  import newMain._
  import MainAnnotation._

  override type Parser[T] = util.CommandLineParser.FromString[T]
  override type Result = Any

  override def command(args: Array[String], commandName: String, documentation: String, parameterInfos: ParameterInfo*) =
    new Command[Parser, Result]:
      private enum ArgumentKind {
        case SimpleArgument, OptionalArgument, VarArgument
      }

      private val argMarker = "--"
      private val shortArgMarker = "-"

      /**
        * The name of the special argument to display the method's help.
        * If one of the method's parameters is called the same, will be ignored.
        */
      private val helpArg = "help"
      private var helpIsOverridden = false

      /**
        * The short name of the special argument to display the method's help.
        * If one of the method's parameters uses the same short name, will be ignored.
        */
      private val shortHelpArg = 'h'
      private var shortHelpIsOverridden = false

      private val maxUsageLineLength = 120

      /** A map from argument canonical name (the name of the parameter in the method definition) to parameter informations */
      private val nameToParameterInfo: Map[String, ParameterInfo] = parameterInfos.map(infos => infos.name -> infos).toMap

      private val (positionalArgs, byNameArgs, invalidByNameArgs) = {
        val namesToCanonicalName: Map[String, String] = parameterInfos.flatMap(
          infos =>
            var names = getAlternativeNames(infos)
            val canonicalName = infos.name
            if nameIsValid(canonicalName) then names = canonicalName +: names
            names.map(_ -> canonicalName)
        ).toMap
        val shortNamesToCanonicalName: Map[Char, String] = parameterInfos.flatMap(
          infos =>
            var names = getShortNames(infos)
            val canonicalName = infos.name
            if shortNameIsValid(canonicalName) then names = canonicalName(0) +: names
            names.map(_ -> canonicalName)
        ).toMap

        helpIsOverridden = namesToCanonicalName.exists((name, _) => name == helpArg)
        shortHelpIsOverridden = shortNamesToCanonicalName.exists((name, _) => name == shortHelpArg)

        def getCanonicalArgName(arg: String): Option[String] =
          if arg.startsWith(argMarker) && arg.length > argMarker.length then
            namesToCanonicalName.get(arg.drop(argMarker.length))
          else if arg.startsWith(shortArgMarker) && arg.length == shortArgMarker.length + 1 then
            shortNamesToCanonicalName.get(arg(shortArgMarker.length))
          else
            None

        def isArgName(arg: String): Boolean =
          val isFullName = arg.startsWith(argMarker)
          val isShortName = arg.startsWith(shortArgMarker) && arg.length == shortArgMarker.length + 1 && shortNameIsValid(arg(shortArgMarker.length))
          isFullName || isShortName

        def recurse(remainingArgs: Seq[String], pa: mutable.Queue[String], bna: Seq[(String, String)], ia: Seq[String]): (mutable.Queue[String], Seq[(String, String)], Seq[String]) =
          remainingArgs match {
            case Seq() =>
              (pa, bna, ia)
            case argName +: argValue +: rest if isArgName(argName) =>
              getCanonicalArgName(argName) match {
                case Some(canonicalName) => recurse(rest, pa, bna :+ (canonicalName -> argValue), ia)
                case None => recurse(rest, pa, bna, ia :+ argName)
              }
            case arg +: rest =>
              recurse(rest, pa :+ arg, bna, ia)
          }

        val (pa, bna, ia) = recurse(args.toSeq, mutable.Queue.empty, Vector(), Vector())
        val nameToArgValues: Map[String, Seq[String]] = if bna.isEmpty then Map.empty else bna.groupMapReduce(_._1)(p => List(p._2))(_ ++ _)
        (pa, nameToArgValues, ia)
      }

      /** The kind of the arguments. Used to display help about the main method. */
      private val argKinds = new mutable.ArrayBuffer[ArgumentKind]

      /** A buffer for all errors */
      private val errors = new mutable.ArrayBuffer[String]

      /** Issue an error, and return an uncallable getter */
      private def error(msg: String): () => Nothing =
        errors += msg
        () => throw new AssertionError("trying to get invalid argument")

      private inline def nameIsValid(name: String): Boolean =
        name.length > 1 // TODO add more checks for illegal characters

      private inline def shortNameIsValid(name: String): Boolean =
        name.length == 1 && shortNameIsValid(name(0))

      private inline def shortNameIsValid(shortName: Char): Boolean =
        ('A' <= shortName && shortName <= 'Z') || ('a' <= shortName && shortName <= 'z')

      private def getNameWithMarker(name: String | Char): String = name match {
        case c: Char => shortArgMarker + c
        case s: String if shortNameIsValid(s) => shortArgMarker + s
        case s => argMarker + s
      }

      private def convert[T](argName: String, arg: String, p: Parser[T]): () => T =
        p.fromStringOption(arg) match
          case Some(t) => () => t
          case None => error(s"invalid argument for $argName: $arg")

      private def usage(): Unit =
        def argsUsage: Seq[String] =
          for ((infos, kind) <- parameterInfos.zip(argKinds))
          yield {
            val canonicalName = getNameWithMarker(infos.name)
            val shortNames = getShortNames(infos).map(getNameWithMarker)
            val alternativeNames = getAlternativeNames(infos).map(getNameWithMarker)
            val namesPrint = (canonicalName +: alternativeNames ++: shortNames).mkString("[", " | ", "]")

            kind match {
              case ArgumentKind.SimpleArgument => s"$namesPrint <${infos.typeName}>"
              case ArgumentKind.OptionalArgument => s"[$namesPrint <${infos.typeName}>]"
              case ArgumentKind.VarArgument => s"[<${infos.typeName}> [<${infos.typeName}> [...]]]"
            }
          }

        def wrapArgumentUsages(argsUsage: Seq[String], maxLength: Int): Seq[String] = {
          def recurse(args: Seq[String], currentLine: String, acc: Vector[String]): Seq[String] =
            (args, currentLine) match {
              case (Nil, "") => acc
              case (Nil, l) => (acc :+ l)
              case (arg +: t, "") => recurse(t, arg, acc)
              case (arg +: t, l) if l.length + 1 + arg.length <= maxLength => recurse(t, s"$l $arg", acc)
              case (arg +: t, l) => recurse(t, arg, acc :+ l)
            }

          recurse(argsUsage, "", Vector()).toList
        }

        val usageBeginning = s"Usage: $commandName "
        val argsOffset = usageBeginning.length
        val usages = wrapArgumentUsages(argsUsage, maxUsageLineLength - argsOffset)

        println(usageBeginning + usages.mkString("\n" + " " * argsOffset))
      end usage

      private def explain(): Unit =
        inline def shiftLines(s: Seq[String], shift: Int): String = s.map(" " * shift + _).mkString("\n")

        def wrapLongLine(line: String, maxLength: Int): List[String] = {
          def recurse(s: String, acc: Vector[String]): Seq[String] =
            val lastSpace = s.trim.nn.lastIndexOf(' ', maxLength)
            if ((s.length <= maxLength) || (lastSpace < 0))
              acc :+ s
            else {
              val (shortLine, rest) = s.splitAt(lastSpace)
              recurse(rest.trim.nn, acc :+ shortLine)
            }

          recurse(line, Vector()).toList
        }

        if (documentation.nonEmpty)
          println(wrapLongLine(documentation, maxUsageLineLength).mkString("\n"))
        if (nameToParameterInfo.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for ((infos, kind) <- parameterInfos.zip(argKinds))
            val canonicalName = getNameWithMarker(infos.name)
            val shortNames = getShortNames(infos).map(getNameWithMarker)
            val alternativeNames = getAlternativeNames(infos).map(getNameWithMarker)
            val otherNames = (alternativeNames ++: shortNames) match {
              case Seq() => ""
              case names => names.mkString("(", ", ", ") ")
            }
            val argDoc = StringBuilder(" " * argNameShift)
            argDoc.append(s"$canonicalName $otherNames- ${infos.typeName}")

            kind match {
              case ArgumentKind.OptionalArgument => argDoc.append(" (optional)")
              case ArgumentKind.VarArgument => argDoc.append(" (vararg)")
              case _ =>
            }

            val doc = infos.documentation
            if (doc.nonEmpty) {
              val shiftedDoc =
                doc.split("\n").nn
                    .map(line => shiftLines(wrapLongLine(line.nn, maxUsageLineLength - argDocShift), argDocShift))
                    .mkString("\n")
              argDoc.append("\n").append(shiftedDoc)
            }

            println(argDoc)
        }
      end explain

      private def getAliases(paramInfos: ParameterInfo): Seq[String] =
        paramInfos.annotations.collect{ case a: Alias => a }.flatMap(_.aliases)

      private def getAlternativeNames(paramInfos: ParameterInfo): Seq[String] =
        getAliases(paramInfos).filter(nameIsValid(_))

      private def getShortNames(paramInfos: ParameterInfo): Seq[Char] =
        getAliases(paramInfos).filter(shortNameIsValid(_)).map(_(0))

      private def getInvalidNames(paramInfos: ParameterInfo): Seq[String | Char] =
        getAliases(paramInfos).filter(name => !nameIsValid(name) && !shortNameIsValid(name))

      override def argGetter[T](idx: Int, optDefaultGetter: Option[() => T])(using p: Parser[T]): () => T =
        val name = parameterInfos(idx).name
        argKinds += (if optDefaultGetter.nonEmpty then ArgumentKind.OptionalArgument else ArgumentKind.SimpleArgument)
        val parameterInfo = nameToParameterInfo(name)

        byNameArgs.get(name) match {
          case Some(Nil) =>
            throw AssertionError(s"$name present in byNameArgs, but it has no argument value")
          case Some(argValues) =>
            if argValues.length > 1 then
              // Do not accept multiple values
              // Remove this test to take last given argument
              error(s"more than one value for $name: ${argValues.mkString(", ")}")
            else
              convert(name, argValues.last, p)
          case None =>
            if positionalArgs.length > 0 then
              convert(name, positionalArgs.dequeue, p)
            else if optDefaultGetter.nonEmpty then
              optDefaultGetter.get
            else
              error(s"missing argument for $name")
        }
      end argGetter

      override def varargGetter[T](using p: Parser[T]): () => Seq[T] =
        val name = parameterInfos.last.name
        argKinds += ArgumentKind.VarArgument

        val byNameGetters = byNameArgs.getOrElse(name, Seq()).map(arg => convert(name, arg, p))
        val positionalGetters = positionalArgs.removeAll.map(arg => convert(name, arg, p))
        // First take arguments passed by name, then those passed by position
        () => (byNameGetters ++ positionalGetters).map(_())

      override def run(f: => Result): Unit =
        // Check aliases unicity
        val nameAndCanonicalName = nameToParameterInfo.toList.flatMap {
          case (canonicalName, infos) => (canonicalName +: getAlternativeNames(infos) ++: getShortNames(infos)).map(_ -> canonicalName)
        }
        val nameToCanonicalNames = nameAndCanonicalName.groupMap(_._1)(_._2)

        for (name, canonicalNames) <- nameToCanonicalNames if canonicalNames.length > 1
        do throw IllegalArgumentException(s"$name is used for multiple parameters: ${canonicalNames.mkString(", ")}")

        // Check aliases validity
        val problematicNames = nameToParameterInfo.toList.flatMap((_, infos) => getInvalidNames(infos))
        if problematicNames.length > 0 then throw IllegalArgumentException(s"The following aliases are invalid: ${problematicNames.mkString(", ")}")

        // Handle unused and invalid args
        for (remainingArg <- positionalArgs) error(s"unused argument: $remainingArg")
        for (invalidArg <- invalidByNameArgs) error(s"unknown argument name: $invalidArg")

        val displayHelp =
          (!helpIsOverridden && args.contains(getNameWithMarker(helpArg))) || (!shortHelpIsOverridden && args.contains(getNameWithMarker(shortHelpArg)))

        if displayHelp then
          usage()
          println()
          explain()
        else if errors.nonEmpty then
          for msg <- errors do println(s"Error: $msg")
          usage()
        else
          f
      end run
  end command
end newMain

object newMain:
  @experimental
  final class Alias(val aliases: String*) extends MainAnnotation.ParameterAnnotation
end newMain
