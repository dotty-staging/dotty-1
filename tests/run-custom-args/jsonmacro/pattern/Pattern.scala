package jsonlib
package pattern

import jsonlib.Json.Str

enum Pattern:
  case Obj(namePatterns: (String, Pattern)*)
  case Arr(values: Pattern*)
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Null
  case InterpolatedValue(idx: Int)

  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    // TODO avoid O(n^2) concatenation of results
    this match
      case Obj(namePatterns*) =>
        json match
          case Json.Obj(nameValues) =>
            // TODO improve
            val res = (for (name, pattern) <- namePatterns yield
              nameValues.get(Json.Str(name)) match
                case Some(value) => pattern.unapplySeq(value)
                case None => return None
            ).filter(_.isDefined).flatten.flatten
            Some(res)
          case _ => None
      case Arr(patterns*) =>
        json match
          case Json.Arr(values*) =>
            // TODO improve
            values.zip(patterns).foldLeft[Option[Seq[Json.Value]]](Some(Seq())) {
              case (acc, (value, pattern)) =>
                for extractedAcc <- acc
                    extracted <- pattern.unapplySeq(value)
                yield extractedAcc ++ extracted
            }
          case _ => None
      case Num(value) =>
        json match
          case Json.Num(`value`) => Some(Seq())
          case _ => None
      case Str(value) =>
        json match
          case Json.Str(`value`) => Some(Seq())
          case _ => None
      case Bool(value) =>
        json match
          case Json.Bool(`value`) => Some(Seq())
          case _ => None
      case Null =>
        if json == Json.Null then Some(Seq())
        else None
      case InterpolatedValue(idx) =>
        Some(Seq(json))
