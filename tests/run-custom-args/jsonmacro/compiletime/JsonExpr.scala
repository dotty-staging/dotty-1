package jsonlib.compiletime

import scala.quoted.*

import jsonlib.*
import jsonlib.parser.*
import jsonlib.pattern.*
import jsonlib.schema.*
import jsonlib.util.*
import scala.quoted.runtime.Patterns.patternType

object JsonExpr:
  /*private[jsonmacro]*/ def jsonExpr(jsonStringContext: Expr[JsonStringContext], argsExpr: Expr[Seq[Json]])(using Quotes): Expr[Json] =
    val json = parsed(jsonStringContext)
    val argExprs = argsExpr match
      case Varargs(argExprs) => argExprs
      case _ => quotes.reflect.report.errorAndAbort("Unpacking StringContext.json args is not supported")
    val jsonExpr = toJsonExpr(json, argExprs)
    ExprSchema.refinedType(json, argExprs) match
      case '[t] => '{ $jsonExpr.asInstanceOf[t & Json] }

  def jsonUnapplySeqExpr(jsonStringContext: Expr[JsonStringContext], scrutinee: Expr[Json])(using Quotes): Expr[Option[Seq[Json]]] =
    val json = parsed(jsonStringContext)
    // Exercise: partially evaluate the pattern matching
    '{ ${Expr(json.toPattern)}.unapplySeq($scrutinee) }

  private def parsed(jsonStringContext: Expr[JsonStringContext])(using Quotes): AST =
    jsonStringContext match
      case '{ jsonlib.json($sc) } =>
        val jsonString = sc.valueOrAbort.parts.map(scala.StringContext.processEscapes)
        Parser(jsonString).parse() match
          case Success(json) => json
          case Error(ParseError(msg, part, offset)) =>
            def error(args: Seq[Expr[String]]) =
              import quotes.reflect.*
              val baseOffset = args(part).asTerm.pos.start
              val pos = Position(jsonStringContext.asTerm.pos.sourceFile, baseOffset + offset, baseOffset + offset)
              report.errorAndAbort(msg + s"($part, $offset)", pos)
            sc match
              case '{ new scala.StringContext(${Varargs(args)}: _*) } => error(args)
              case '{     scala.StringContext(${Varargs(args)}: _*) } => error(args)
              case _ =>
                quotes.reflect.report.errorAndAbort("string context is not known statically")

  private def toJsonExpr(ast: AST, args: Seq[Expr[Json]])(using Quotes): Expr[Json] =
    ast match
      case AST.Null => '{ null }
      case AST.Bool(value) => Expr(value)
      case AST.Num(value) => Expr(value)
      case AST.Str(value) => Expr(value)
      case AST.Arr(value*) => '{ JsonArray(${Varargs(value.map(toJsonExpr(_, args)))}*) }
      case AST.Obj(nameValues*) =>
        val nameValueExprs = for (name, value) <- nameValues yield '{ (${Expr(name)}, ${toJsonExpr(value, args)}) }
        '{ JsonObject(Map(${Varargs(nameValueExprs)}*)) }
      case AST.InterpolatedValue(idx) => args(idx)

  given ToExpr[Pattern] with
    def apply(pattern: Pattern)(using Quotes): Expr[Pattern] =
      pattern match
        case Pattern.Null => '{ Pattern.Null }
        case Pattern.Bool(value) => '{ Pattern.Bool(${Expr(value)}) }
        case Pattern.Num(value) => '{ Pattern.Num(${Expr(value)}) }
        case Pattern.Str(value) => '{ Pattern.Str(${Expr(value)}) }
        case Pattern.Arr(values*) => '{ Pattern.Arr(${Expr(values)}*) }
        case Pattern.Obj(nameValue*) => '{ Pattern.Obj(${Expr(nameValue)}*) }
        case Pattern.InterpolatedValue(idx) => '{ Pattern.InterpolatedValue(${Expr(idx)}) }
