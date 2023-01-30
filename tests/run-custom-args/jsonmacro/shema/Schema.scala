package jsonlib.schema

enum Schema:
  case Value
  case Obj(nameSchemas: (String, Schema)*)
  case Arr
  case Str
  case Num
  case Bool
  case Null
