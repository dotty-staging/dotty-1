package jsonlib

import scala.language.dynamics

import parser.*
import util.*

type Json = JsonObject | JsonArray | Double | String | Boolean | Null

object Json:
  def apply(json: String): Json =
    new Parser(Seq(json)).parse() match
      case Success(ast) => fromAST(ast)
      case Error(ParseError(msg, 0, offset)) =>
        ???

  private def fromAST(ast: AST): Json =
    ast match
      case AST.Null => null
      case AST.Bool(value) => value
      case AST.Num(value) => value
      case AST.Str(value) => value
      case AST.Arr(values*) => JsonArray(values.map(fromAST)*)
      case AST.Obj(nameValues*) =>
        val nameJsons = for (name, value) <- nameValues yield (name, fromAST(value))
        JsonObject(Map(nameJsons*))
