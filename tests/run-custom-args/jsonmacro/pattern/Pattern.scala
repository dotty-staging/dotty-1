package jsonlib
package pattern

enum Pattern:
  case Obj(namePatterns: (String, Pattern)*)
  case Arr(values: Pattern*)
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Null
  case InterpolatedValue(idx: Int)

  def unapplySeq(json: Json): Option[Seq[Json]] =
    // TODO avoid O(n^2) concatenation of results
    this match
      case Obj(namePatterns*) =>
        json match
          case json: JsonObject =>
            // TODO improve
            val res = (for (name, pattern) <- namePatterns yield
              json.nameValues.get(name) match
                case Some(value) => pattern.unapplySeq(value)
                case None => return None
            ).filter(_.isDefined).flatten.flatten
            Some(res)
          case _ => None
      case Arr(patterns*) =>
        json match
          case values: Seq[Json] =>
            // TODO improve
            values.zip(patterns).foldLeft[Option[Seq[jsonlib.Json]]](Some(Seq())) {
              case (acc, (value, pattern)) =>
                for extractedAcc <- acc
                    extracted <- pattern.unapplySeq(value)
                yield extractedAcc ++ extracted
            }
          case _ => None
      case Num(value) =>
        if json == value then Some(Seq()) else None
      case Str(value) =>
        if json == value then Some(Seq()) else None
      case Bool(value) =>
        if json == value then Some(Seq()) else None
      case Null =>
        if json == null then Some(Seq()) else None
      case InterpolatedValue(idx) =>
        Some(Seq(json))
