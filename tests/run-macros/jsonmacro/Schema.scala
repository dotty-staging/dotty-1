package jsonmacro

private enum Schema:
  case Value
  case Obj(nameSchemas: Map[String, Schema])
  case Arr
  case Str
  case Num
  case Bool
  case Null

object Schema:
  def apply(value: Parsed.Value): Schema =
    value match
      case Parsed.Obj(nameValues) =>
        Schema.Obj(nameValues.map((k, v) => k.value -> Schema(v)))
      case Parsed.Arr(_*) => Schema.Arr
      case Parsed.Str(_) => Schema.Str
      case Parsed.Num(_) => Schema.Num
      case Parsed.Bool(_) => Schema.Bool
      case Parsed.Null => Schema.Null
      case Parsed.InterpolatedValue(_) =>
        Schema.Value // TODO refine further
