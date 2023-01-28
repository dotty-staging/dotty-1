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
  def apply(value: interpolated.Value): Schema =
    value match
      case interpolated.Obj(nameValues) =>
        Schema.Obj(nameValues.map((k, v) => k.value -> Schema(v)))
      case interpolated.Arr(_*) => Schema.Arr
      case interpolated.Str(_) => Schema.Str
      case interpolated.Num(_) => Schema.Num
      case interpolated.Bool(_) => Schema.Bool
      case interpolated.Null => Schema.Null
      case interpolated.InterpolatedValue => Schema.Value // TODO refine further