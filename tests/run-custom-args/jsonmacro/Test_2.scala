import jsonlib.*

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


  val user = json"""{
    "firstName": "John",
    "lastName": "Doe"
  }"""
  val bool = json"true"
  val account = json"""{
    "user": $user,
    "active": $bool
  }"""
  account.active.value
  account.user.firstName


  (account: Json.Value) match
    case json"""{ "user": $x, "a": true }""" => println("case 1: " + x)
    case json"""{ "user": $x, "active": $y }""" => println("case 2: " + (x, y))

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
