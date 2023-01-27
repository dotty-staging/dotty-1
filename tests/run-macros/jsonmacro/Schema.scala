package jsonmacro

private enum Schema:
  case Value
  case Obj(nameSchemas: Map[String, Schema])
  case Arr
  case Str
  case Num
  case Bool
  case Null

  def of(json: Json.Value): Schema =
    json match
      case Json.Obj(nameValues) =>
        Schema.Obj(nameValues.map((k, v) => k.value -> of(v)))
      case _: Json.Arr => Schema.Arr
      case _: Json.Str => Schema.Str
      case _: Json.Num => Schema.Num
      case _: Json.Bool => Schema.Bool
      case Json.Null => Schema.Null
