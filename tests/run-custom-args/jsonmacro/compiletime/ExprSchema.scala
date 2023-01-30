package jsonmacro
package compiletime

import scala.quoted.*

import jsonmacro.parser.*
import jsonmacro.schema.*
import jsonmacro.util.*

private object ExprSchema:

  def refinedType(json: Parsed.Value, args: Seq[Expr[Json.Value]])(using Quotes): Type[?] =
    refinedType(schema(json, args))

  private def refinedType(schema: Schema)(using Quotes): Type[?] =
    schema match
      case Schema.Value => Type.of[Json.Value]
      case Schema.Obj(nameSchemas*) =>
        import quotes.reflect.*
        nameSchemas.foldLeft(TypeRepr.of[Json.Obj]) { case (acc, (name, schema)) =>
          Refinement(acc, name, TypeRepr.of(using refinedType(schema)))
        }.asType
      case Schema.Arr => Type.of[Json.Arr]
      case Schema.Str => Type.of[Json.Str]
      case Schema.Num => Type.of[Json.Num]
      case Schema.Bool => Type.of[Json.Bool]
      case Schema.Null => Type.of[Json.Null.type]

  private def schema(value: Parsed.Value, args: Seq[Expr[Json.Value]])(using Quotes): Schema =
    value match
      case Parsed.Obj(nameValues*) =>
        val nameSchemas = for (name, value) <- nameValues yield (name, schema(value, args))
        Schema.Obj(nameSchemas*)
      case Parsed.Arr(_*) => Schema.Arr
      case Parsed.Str(_) => Schema.Str
      case Parsed.Num(_) => Schema.Num
      case Parsed.Bool(_) => Schema.Bool
      case Parsed.Null => Schema.Null
      case Parsed.InterpolatedValue(idx) =>
        args(idx) match
          case '{ $x : t } => schemaOf[t]

  private def schemaOf[T : Type](using Quotes): Schema =
    Type.of[T] match
      case '[Json.Null.type] => Schema.Null
      case '[Json.Bool] => Schema.Bool
      case '[Json.Str] => Schema.Str
      case '[Json.Num] => Schema.Num
      case '[Json.Arr] => Schema.Arr
      case '[Json.Obj] =>
        import quotes.reflect.*
        def refinements(tpe: TypeRepr): Vector[(String, Schema)] =
          tpe match
            case Refinement(parent, name, info) =>
              val  refinedSchema = info.asType match
                case '[t] => schemaOf[t]
              refinements(parent) :+ (name, refinedSchema)
            case _ => Vector()
        Schema.Obj(refinements(TypeRepr.of[T].widenTermRefByName)*)
      case _ => Schema.Value
