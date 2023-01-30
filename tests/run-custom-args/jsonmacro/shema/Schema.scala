package jsonmacro.schema

enum Schema:
  case Value
  case Obj(nameSchemas: Map[String, Schema])
  case Arr
  case Str
  case Num
  case Bool
  case Null
