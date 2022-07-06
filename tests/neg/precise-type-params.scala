import annotation.precise

def normalTypeParam() =
  def id[T](x: T): T = x
  val x: 3 = id(3)
  val y = id(3)
  val z: 3 = y // error

  def same[T](x: T, y: T) = true
  same(1, 2)

  class Box[T](x: T)
  val c: Boolean = ???
  val b /*: Box[Int]*/ = Box(if c then 2 else 3)

def singletonBound() =
  def id[T <: Singleton](x: T): T = x
  val x: 3 = id(3)
  val y = id(3)
  val z: 3 = y // error

  def same[T <: Singleton](x: T, y: T) = true
  same(1, 2)

  class Box[T <: Singleton](x: T)
  val c: Boolean = ???
  val b /*: Box[2 | 3]*/ = Box(if c then 2 else 3)


def preciseAnnotation() =
  def id[T <: Any @precise](x: T): T = x
  val x: 3 = id(3)
  val y = id(3)
  val z: 3 = y // error

  def same[T <: Any @precise ](x: T, y: T) = true
  same(1, 2)

  class Box[T <: Any @precise](x: T)
  val c: Boolean = ???
  val b /*: Box[2 | 3]*/ = Box(if c then 2 else 3)

def termRef() =
  def id(x: Any): x.type = x
  val x: 3 = id(3)
  val y = id(3)
  val z: 3 = y // error

  def same(x: Any, y: x.type) = true
  same(1, 2) // error
