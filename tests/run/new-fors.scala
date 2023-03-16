import scala.language.experimental.newFors

def for1 =
  for {
    a = 1
    b <- List(a, 2)
    c <- List(3, 4)
  } yield (b, c)

def for2 =
  for {
    a = 1
    b <- List(a, 2)
    c <- List('a')
    enact List(3, 4)
    d <- List(5, 6)
  } yield (b, c, d)

def for3 =
  for {
    a = 1
    enact List(a, 2)
    a <- List('a')
  } yield a

def for4 =
  for {
    a = 1
    enact List(a, 2)
  } yield a

object Test extends App {
  println(for1)
  println(for2)
  println(for3)
  println(for4)
}
