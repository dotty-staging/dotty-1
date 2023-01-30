package jsonlib.parser

import jsonlib.util.*
import jsonlib.pattern.*

private[jsonlib] enum AST:
  case Obj(nameValues: (String, AST)*)
  case Arr(values: AST*)
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Null
  case InterpolatedValue(idx: Int)

  def toPattern: ValuePattern =
    this match
      case Null => NullPattern
      case Bool(value) => BoolPattern(value)
      case Num(value) => NumPattern(value)
      case Str(value) => StrPattern(value)
      case Arr(values*) =>
        val patterns = values.map(_.toPattern)
        ArrPattern(patterns*)
      case Obj(nameValues*) =>
        val namePatterns = for (name, value) <- nameValues yield (name, value.toPattern)
        ObjPattern(namePatterns*)
      case InterpolatedValue(idx) =>
        InterpolatedValuePattern(idx)
