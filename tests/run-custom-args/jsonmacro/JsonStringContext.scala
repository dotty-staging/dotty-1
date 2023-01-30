package jsonlib

import jsonlib.compiletime.JsonExpr.{jsonExpr, jsonUnapplySeqExpr}

extension (stringContext: scala.StringContext)
  inline def json: JsonStringContext =
    scala.compiletime.error("Json.json should have been removed by macro")

type JsonStringContext

object JsonStringContext:

  extension (inline jsonStringContext: JsonStringContext)
    transparent inline def apply(inline args: Json.Value*): Json.Value =
      ${ jsonExpr('jsonStringContext, 'args) }

    transparent inline def unapplySeq(scrutinee: Json.Value): Option[Seq[Json.Value]] =
      ${ jsonUnapplySeqExpr('jsonStringContext, 'scrutinee) }: Option[Seq[Json.Value]]
