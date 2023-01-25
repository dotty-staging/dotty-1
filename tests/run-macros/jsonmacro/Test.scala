import jsonmacro.*

@main def Test: Unit =
  val n: Json.Null.type = json"null"
  val t: Json.Bool = json"true"
  val f: Json.Bool = json"false"
  val o: Json.Obj = json"{}"
  val a: Json.Arr = json"[]"

  json" null "
  json"[true]"
  json"[true, true]"

