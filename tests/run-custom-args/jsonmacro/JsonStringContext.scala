package jsonlib

import jsonlib.compiletime.JsonExpr.{jsonExpr, jsonUnapplySeqExpr}

extension (stringContext: scala.StringContext)
  inline def json: JsonStringContext =
    scala.compiletime.error("Json.json should have been removed by macro")

type JsonStringContext

object JsonStringContext:

  extension (inline jsonStringContext: JsonStringContext)
    transparent inline def apply(inline args: Json*): Json =
      ${ jsonExpr('jsonStringContext, 'args) }

    transparent inline def unapplySeq(scrutinee: Json): Option[Seq[Json]] =
      ${ jsonUnapplySeqExpr('jsonStringContext, 'scrutinee) }
