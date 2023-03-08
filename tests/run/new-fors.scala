import scala.language.experimental.newFors

// def for1 =
//   for {
//     a = 1
//     b <- List(a, 2)
//     c <- List(3, 4)
//   } yield (b, c)

def for2 =
  for {
    a = 1
    b <- List(a, 2)
    c <- List('a')
    enact List(3, 4)
    d <- List(5, 6)
  } yield (b, c, d)

object Test extends App {
  // println(for1)
  println(for2)
}
