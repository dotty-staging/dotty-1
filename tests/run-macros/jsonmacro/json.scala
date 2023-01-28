package jsonmacro

import scala.language.dynamics
import scala.compiletime.ops.string

import scala.quoted.*
import result.*

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
    transparent inline def json(args: Json.Value*): Json.Value = ${ jsonExpr('stringContext, 'args) }

  // TODO add arguments
  private def jsonExpr(stringContext: Expr[StringContext], args: Expr[Seq[Json.Value]])(using Quotes): Expr[Json.Value] =
    val jsonString = stringContext.valueOrAbort.parts.map(StringContext.processEscapes)
    Parser(jsonString).parse() match
      case Success(json) =>
        val jsonExpr = toJsonExpr(json, args)
        refinedType(Schema(json)) match
          case '[t] => '{ $jsonExpr.asInstanceOf[t & Json.Value] }
      case Error(ParseError(msg, part, offset)) =>
        import quotes.reflect.*
        val baseOffset = stringContext.asTerm.pos.start // TODO support """ and splices
        val pos = Position(stringContext.asTerm.pos.sourceFile, baseOffset + offset, baseOffset + offset)
        report.errorAndAbort(msg + s"($part, $offset)", pos)

  def toJsonExpr(json: interpolated.Value, args: Expr[Seq[Json.Value]])(using Quotes): Expr[Json.Value] =
    json match
      case interpolated.Null => '{ Json.Null }
      case interpolated.Bool(value) => '{ Json.Bool(${Expr(value)}) }
      case interpolated.Num(value) => '{ Json.Num(${Expr(value)}) }
      case interpolated.Str(value) => '{ Json.Str(${Expr(value)}) }
      case interpolated.Arr(value*) => '{ Json.Arr(${Varargs(value.map(toJsonExpr(_, args)))}*) }
      case interpolated.Obj(value) =>
        // TODo improve
        def f(k: interpolated.Str, v: interpolated.Value) = '{ (Json.Str(${Expr(k.value)}), ${toJsonExpr(v, args)}) }
        '{ Json.Obj(Map(${Varargs(value.toSeq.map(f))}*)) }
      case interpolated.InterpolatedValue(idx) =>
        '{ $args(${Expr(idx)}) }

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
