import annotation.refined

def p(x: Int): Boolean = ???
type P = Int @refined[Int](p)
def takeP(x: P): P = x

def q(x: Int): Boolean = ???
type Q = Int @refined[Int](q)
def takeQ(x: Q): Q = x

def takePQ(x: P & Q): Q = x

def id[T](x: T): T = x
def idByName[T](x: => T): T = x

def app[T](x: T, f: T => Unit): Unit = f(x)
def appBoth[T](a: T, b: T, f: T => Unit): Unit = {f(a); f(b)}

def test() =
  val x1: P = 1.asInstanceOf
  val x2: P = id(x1)
  val x3: P = idByName(x1)
  def x4: P = 1.asInstanceOf
  val x5: P = id(x4)
  val x6: P = idByName(x4)
  def x7(): P = ???
  val x8: P = id(x7())
  val x9: P = idByName(x7())

  val f: P => P = x => x
  def g(x: P): P = x
  val l1: List[P] = List(x1).map(f)
  val l2: List[P] = List(x1).map(g)
  val l3: List[P] = List(x1).map(id)
  val l4: List[P] = List(x1, x2).map(f)
  val l5: List[P] = List(x1, x2).map(g)
  val l6: List[P] = List(x1, x2).map(id)


  val x10: P & Q = 1.asInstanceOf
  val x11: Int @refined[Int](p) @refined[Int](q) = 1.asInstanceOf
  val x12: P & Q = x11
  app(x1, takeP)
  app(x10, takeP)
  app(x11, takeP)
  app(x10, takePQ)
  app(x11, takePQ)
  appBoth(x1, x10, takeP)
  appBoth(x1, x11, takeP)
  appBoth(x10, x1, takeP)
  appBoth(x11, x1, takeP)
  appBoth(x10, x11, takeP)
  appBoth(x1, x10, takePQ) // Only works because we are not checking predicates
  appBoth(x10, x11, takePQ)
