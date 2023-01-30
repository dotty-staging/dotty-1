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

  def toPattern: Pattern =
    this match
      case Null => Pattern.Null
      case Bool(value) => Pattern.Bool(value)
      case Num(value) => Pattern.Num(value)
      case Str(value) => Pattern.Str(value)
      case Arr(values*) =>
        val patterns = values.map(_.toPattern)
        Pattern.Arr(patterns*)
      case Obj(nameValues*) =>
        val namePatterns = for (name, value) <- nameValues yield (name, value.toPattern)
        Pattern.Obj(namePatterns*)
      case InterpolatedValue(_) =>
        Pattern.InterpolatedValue
