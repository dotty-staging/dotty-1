object Test:
  val xs = List(1, 2, 3, 4, 5)
  xs match {
    case List(1, 2, xs2 as xs1: _*) => println(xs2) // error // error
    case _ => ()
  }