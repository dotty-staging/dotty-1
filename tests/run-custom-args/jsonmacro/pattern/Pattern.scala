package jsonlib
package pattern

import jsonlib.util.optional, optional.?

enum Pattern:
  case Obj(namePatterns: (String, Pattern)*)
  case Arr(values: Pattern*)
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Null
  case InterpolatedValue

  def unapplySeq(json: Json): Option[Seq[Json]] =
    // TODO avoid O(n^2) concatenation of results
    this match
      case Obj(namePatterns*) =>
        json match
          case json: JsonObject =>
            optional:
              namePatterns.foldLeft(Seq.empty[Json]) {
                case (acc, (name, pattern)) =>
                  val value = json.nameValues.get(name).?
                  acc ++ pattern.unapplySeq(value).?
              }
          case _ => None
      case Arr(patterns*) =>
        json match
          case values: Seq[Json] =>
            optional:
              values.zip(patterns).foldLeft(Seq.empty[Json]) {
                case (acc, (value, pattern)) =>
                  acc ++ pattern.unapplySeq(value).?
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
      case InterpolatedValue =>
        Some(Seq(json))
