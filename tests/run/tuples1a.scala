object Test extends App {
  val t7 = '5' *: 4 *: "C" *: Tuple0()

  val t7a = t7.tail
  val t7b = t7a.tail
  val t7c: Unit = (t7.tail: (Int, String)).tail
  val t7d: Unit = (t7.tail: Int *: String *: Tuple0).tail
  val t7e: Unit = t7.tail.tail
}
