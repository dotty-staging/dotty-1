package jsonlib

import scala.language.dynamics

import parser.*
import util.*

sealed trait Json
final case class Obj(value: Map[Str, Json]) extends Json, scala.Selectable:
  def selectDynamic(name: String): Json | Json.Undefined.type = value.getOrElse(Str(name), Json.Undefined)
final case class Arr(values: Json*) extends Json
final case class Num(value: Double) extends Json
final case class Str(value: String) extends Json
final case class Bool(value: Boolean) extends Json
case object Null extends Json

object Json:
  object Undefined

  def apply(json: String): Json =
    new Parser(Seq(json)).parse() match
      case Success(ast) => fromAST(ast)
      case Error(ParseError(msg, 0, offset)) =>
        ???

  private def fromAST(ast: AST): Json =
    ast match
      case AST.Null => Null
      case AST.Bool(value) => Bool(value)
      case AST.Num(value) => Num(value)
      case AST.Str(value) => Str(value)
      case AST.Arr(values*) => Arr(values.map(fromAST)*)
      case AST.Obj(nameValues*) =>
        val nameJsons = for (name, value) <- nameValues yield (Str(name), fromAST(value))
        Obj(Map(nameJsons*))
