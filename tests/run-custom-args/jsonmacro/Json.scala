package jsonmacro

import scala.language.dynamics

import jsonmacro.parser.*
import jsonmacro.util.*

object Json:
  sealed trait Value
  final case class Obj(value: Map[Json.Str, Json.Value]) extends Value, scala.Selectable:
    def selectDynamic(name: String): Value | Undefined.type = value.getOrElse(Json.Str(name), Undefined)
  final case class Arr(values: Json.Value*) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  final case class Bool(value: Boolean) extends Value
  case object Null extends Value
  object Undefined

  def apply(json: String): Json.Value =
    new Parser(Seq(json)).parse() match
      case Success(parsed) => fromParsed(parsed)
      case Error(ParseError(msg, 0, offset)) =>
        ???

  private def fromParsed(parsed: Parsed.Value): Json.Value =
    parsed match
      case Parsed.Null => Json.Null
      case Parsed.Bool(value) => Json.Bool(value)
      case Parsed.Num(value) => Json.Num(value)
      case Parsed.Str(value) => Json.Str(value)
      case Parsed.Arr(values*) => Json.Arr(values.map(fromParsed)*)
      case Parsed.Obj(nameValues*) =>
        val nameJsons = for (name, value) <- nameValues yield (Json.Str(name), fromParsed(value))
        Json.Obj(Map(nameJsons*))
