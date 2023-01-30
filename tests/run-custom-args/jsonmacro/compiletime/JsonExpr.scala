package jsonmacro
package compiletime

import scala.quoted.*

import jsonmacro.parser.*
import jsonmacro.schema.*
import jsonmacro.util.*

object JsonExpr:
  /*private[jsonmacro]*/ def jsonExpr(stringContext: Expr[StringContext], argsExpr: Expr[Seq[Json.Value]])(using Quotes): Expr[Json.Value] =
    val jsonString = stringContext.valueOrAbort.parts.map(StringContext.processEscapes)
    Parser(jsonString).parse() match
      case Success(json) =>
        val argExprs = argsExpr match
          case Varargs(argExprs) => argExprs
          case _ => quotes.reflect.report.errorAndAbort("Unpacking StringContext.json args is not supported")
        val jsonExpr = toJsonExpr(json, argExprs)
        ExprSchema.refinedType(json, argExprs) match
          case '[t] => '{ $jsonExpr.asInstanceOf[t & Json.Value] }
      case Error(ParseError(msg, part, offset)) =>
        def error(args: Seq[Expr[String]]) =
          import quotes.reflect.*
          val baseOffset = args(part).asTerm.pos.start
          val pos = Position(stringContext.asTerm.pos.sourceFile, baseOffset + offset, baseOffset + offset)
          report.errorAndAbort(msg + s"($part, $offset)", pos)
        stringContext match
          case '{ new StringContext(${Varargs(args)}: _*) } => error(args)
          case '{     StringContext(${Varargs(args)}: _*) } => error(args)
          case _ =>
            quotes.reflect.report.errorAndAbort("string context is not known statically")
        // report.errorAndAbort(msg + s"($part, $offset)", pos)

  private def toJsonExpr(json: Parsed.Value, args: Seq[Expr[Json.Value]])(using Quotes): Expr[Json.Value] =
    json match
      case Parsed.Null => '{ Json.Null }
      case Parsed.Bool(value) => '{ Json.Bool(${Expr(value)}) }
      case Parsed.Num(value) => '{ Json.Num(${Expr(value)}) }
      case Parsed.Str(value) => '{ Json.Str(${Expr(value)}) }
      case Parsed.Arr(value*) => '{ Json.Arr(${Varargs(value.map(toJsonExpr(_, args)))}*) }
      case Parsed.Obj(value) =>
        // TODo improve
        def f(k: Parsed.Str, v: Parsed.Value) = '{ (Json.Str(${Expr(k.value)}), ${toJsonExpr(v, args)}) }
        '{ Json.Obj(Map(${Varargs(value.toSeq.map(f))}*)) }
      case Parsed.InterpolatedValue(idx) => args(idx)
