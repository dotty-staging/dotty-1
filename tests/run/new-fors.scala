import scala.language.experimental.newFors

def for1 =
  for {
    a = 1
    b <- List(a, 2)
    c <- List(3, 4)
  } yield (b, c)

object Test extends App {
  println(for1)
}
