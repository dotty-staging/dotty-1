package jsonmacro

import scala.language.dynamics

import jsonmacro.compiletime.JsonExpr.jsonExpr

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

  extension (inline stringContext: StringContext)
    transparent inline def json(inline args: Json.Value*): Json.Value =
      ${ jsonExpr('stringContext, 'args) }
