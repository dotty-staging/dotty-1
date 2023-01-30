package jsonlib
package pattern

import jsonlib.Json.Str


sealed trait ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]]

final case class ObjPattern(namePatterns: (String, ValuePattern)*) extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
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

final case class ArrPattern(values: ValuePattern*) extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    json match
      case Json.Arr(values*) =>
        // TODO improve
        values.zip(this.values).foldLeft[Option[Seq[Json.Value]]](Some(Seq())) {
          case (acc, (value, pattern)) =>
            for extractedAcc <- acc
                extracted <- pattern.unapplySeq(value)
            yield extractedAcc ++ extracted
        }
      case _ => None

final case class NumPattern(value: Double) extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    json match
      case Json.Num(`value`) => Some(Seq())
      case _ => None

final case class StrPattern(value: String) extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    json match
      case Json.Str(`value`) => Some(Seq())
      case _ => None

final case class BoolPattern(value: Boolean) extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    json match
      case Json.Bool(`value`) => Some(Seq())
      case _ => None


case object NullPattern extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    if json == Json.Null then Some(Seq()) else None

final case class InterpolatedValuePattern(idx: Int) extends ValuePattern:
  def unapplySeq(json: Json.Value): Option[Seq[Json.Value]] =
    Some(Seq(json))

