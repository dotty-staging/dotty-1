package jsonlib

import scala.language.dynamics

import parser.*
import util.*

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
      case Success(ast) => fromAST(ast)
      case Error(ParseError(msg, 0, offset)) =>
        ???

  private def fromAST(ast: AST): Json.Value =
    ast match
      case AST.Null => Json.Null
      case AST.Bool(value) => Json.Bool(value)
      case AST.Num(value) => Json.Num(value)
      case AST.Str(value) => Json.Str(value)
      case AST.Arr(values*) => Json.Arr(values.map(fromAST)*)
      case AST.Obj(nameValues*) =>
        val nameJsons = for (name, value) <- nameValues yield (Json.Str(name), fromAST(value))
        Json.Obj(Map(nameJsons*))
