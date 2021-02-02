enum E extends Enum[Nothing] { case X }

@main def Test: Unit =
  val res0: Array[E] = E.values
  val res1: Array[Nothing] = E.X.getDeclaringClass.getEnumConstants
  assert(res0.sameElements(res1))
