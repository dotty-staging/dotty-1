package jsonmacro

import scala.language.dynamics
import scala.compiletime.ops.string

import scala.quoted.*

sealed trait Json

object Json:
  final case class Obj(value: Map[Json.Str, Json]) extends Json, scala.Dynamic:
    def selectDynamic(name: String): Json | Undefined.type = value.getOrElse(Json.Str(name), Undefined)
  final case class Arr(values: Json*) extends Json
  final case class Num(value: Double) extends Json
  final case class Str(value: String) extends Json
  final case class Bool(value: Boolean) extends Json
  case object Null extends Json
  object Undefined

  given ToExpr[Json] with
    def apply(x: Json)(using Quotes): Expr[Json] =
      x match
        case Json.Null => '{ Json.Null }
        case Json.Bool(value) => '{ Json.Bool(${Expr(value)}) }
        case Json.Num(value) => '{ Json.Num(${Expr(value)}) }
        case x: Json.Str => Expr(x)
        case Json.Arr(value*) => '{ Json.Arr(${Expr(value)}*) }
        case Json.Obj(value) => '{ Json.Obj(${Expr(value)}) }

  given ToExpr[Json.Str] with
    def apply(x: Json.Str)(using Quotes): Expr[Json.Str] =
      '{ Json.Str(${Expr(x.value)}) }

extension (inline stringContext: StringContext)
  transparent inline def json(): Json = ${ jsonExpr('stringContext) }

private def jsonExpr(stringContext: Expr[StringContext])(using Quotes): Expr[Json] =
  import Parser.*
  val jsonString = stringContext.valueOrAbort.parts.mkString
  Parser(jsonString).parse() match
    case Parsed(json) => Expr(json)
    case Error(msg, offset) =>
      import quotes.reflect.*
      val baseOffset = stringContext.asTerm.pos.start // TODO support """ and splices
      val pos = Position(stringContext.asTerm.pos.sourceFile, baseOffset + offset, baseOffset + offset)
      report.errorAndAbort(msg, pos)
