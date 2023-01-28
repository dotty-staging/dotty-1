import jsonmacro.*
import jsonmacro.Json.json

@main def Test: Unit =
  val n: Json.Null.type = json"null"
  val t: Json.Bool = json"true"
  val f: Json.Bool = json"false"
  val o: Json.Obj = json"{}"
  val a: Json.Arr = json"[]"
  val s: Json.Str = json"""""""" // empty string
  val s2: Json.Str = json"\"\"" // empty string

  println(json" null ")
  println(json"[true]")
  println(json"[true, true]")
  println(json"""{ "name": true }""".name)
  println(json"""{ "name": true, "name2": false }""")

  println(json"${n}")
  println(json"[${t}]")
  println(json"[${f}, ${o}]")
  println(json"""{ "a": ${a}, "b": ${s}}""")

  // json"n u l l"
  // json"""n u l l"""
  // json"a"
  // json"a${n}"
  // json"[${n}a ${n}"
  // json"[${n},a ${n}"
  // json"[${n} ${n}"
  // json"[${n} ${n},"
  // json"[true, "
  // json"[true, ,"
