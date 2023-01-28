import jsonmacro.*

@main def Test: Unit =
  val n: Json.Null.type = json"null"
  val t: Json.Bool = json"true"
  val f: Json.Bool = json"false"
  val o: Json.Obj = json"{}"
  val a: Json.Arr = json"[]"
  val s: Json.Str = json"""""""" // empty string
  val s2: Json.Str = json"\"\"" // empty string

  json" null "
  json"[true]"
  json"[true, true]"
  json"""{ "name": true }""".name
  json"""{ "name": true, "name2": false }"""

  json"${n}"
  json"[${n}]"
  json"[${n}, ${n}]"
  json"""{ "a": ${n}, "b": ${n}}"""
  // json"n u l l"
  // json"a"
  // json"[true, "
  // json"[true, ,"

