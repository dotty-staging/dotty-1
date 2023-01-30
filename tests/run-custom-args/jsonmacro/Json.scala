package jsonmacro

import scala.language.dynamics

import jsonmacro.parser.*
import jsonmacro.util.*
import jsonmacro.compiletime.JsonExpr.{jsonExpr, jsonUnapplyExpr}

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


  type StringContext
  object StringContext:
    def apply(sc: scala.StringContext): StringContext = sc.asInstanceOf[StringContext]

  extension (stringContext: scala.StringContext)
    inline def json: StringContext =
      scala.compiletime.error("Json.json should have been removed by macro")

  extension (inline stringContext: StringContext)
    transparent inline def apply(inline args: Json.Value*): Json.Value =
      ${ jsonExpr('stringContext, 'args) }

    transparent inline def unapply(scrutinee: Json.Value): Option[Any] =
      ${ jsonUnapplyExpr('stringContext) }


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
