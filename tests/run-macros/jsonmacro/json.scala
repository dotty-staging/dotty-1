package jsonmacro

import scala.language.dynamics
import scala.compiletime.ops.string

import scala.quoted.*
import result.*

object Json:
  sealed trait Value
  final case class Obj(value: Map[Json.Str, Json.Value]) extends Value, scala.Dynamic:
    def selectDynamic(name: String): Value | Undefined.type = value.getOrElse(Json.Str(name), Undefined)
  final case class Arr(values: Json.Value*) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  final case class Bool(value: Boolean) extends Value
  case object Null extends Value
  object Undefined

  given ToExpr[Json.Value] with
    def apply(x: Json.Value)(using Quotes): Expr[Json.Value] =
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
  // TODO add arguments
  transparent inline def json(/* args: Json.Value* */): Json.Value = ${ jsonExpr('stringContext) }

private def jsonExpr(stringContext: Expr[StringContext])(using Quotes): Expr[Json.Value] =
  val jsonString = stringContext.valueOrAbort.parts.map(StringContext.processEscapes).mkString
  Parser(jsonString).parse() match
    case Success(json) =>
      val jsonExpr = Expr(json)
      refinedType(Schema.of(json)) match
        case '[t] => '{ $jsonExpr.asInstanceOf[t & Json.Value] }
    case Error(ParseError(msg, offset)) =>
      import quotes.reflect.*
      val baseOffset = stringContext.asTerm.pos.start // TODO support """ and splices
      val pos = Position(stringContext.asTerm.pos.sourceFile, baseOffset + offset, baseOffset + offset)
      report.errorAndAbort(msg, pos)

private def refinedType(schema: Schema)(using Quotes): Type[?] =
  schema match
    case Schema.Value => Type.of[Json.Value]
    case Schema.Obj(nameSchemas: Map[String, Schema]) =>
      import quotes.reflect.*
      nameSchemas.foldLeft(TypeRepr.of[Json.Obj]) { case (acc, (name, schema)) =>
        Refinement(acc, name, TypeRepr.of(using refinedType(schema)))
      }.asType
    case Schema.Arr => Type.of[Json.Arr]
    case Schema.Str => Type.of[Json.Str]
    case Schema.Num => Type.of[Json.Num]
    case Schema.Bool => Type.of[Json.Bool]
    case Schema.Null => Type.of[Json.Null.type]
