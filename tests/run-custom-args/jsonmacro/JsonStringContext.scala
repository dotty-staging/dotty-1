package jsonlib

import jsonlib.compiletime.JsonExpr.{jsonExpr, jsonUnapplyExpr}

extension (stringContext: scala.StringContext)
  inline def json: JsonStringContext =
    scala.compiletime.error("Json.json should have been removed by macro")

type JsonStringContext

object JsonStringContext:

  extension (inline jsonStringContext: JsonStringContext)
    transparent inline def apply(inline args: Json.Value*): Json.Value =
      ${ jsonExpr('jsonStringContext, 'args) }

    transparent inline def unapply(scrutinee: Json.Value): Option[Any] =
      ${ jsonUnapplyExpr('jsonStringContext) }
