import scala.annotation.{experimental, TastyAnnotation}
import scala.quoted._

class Foo:
  @experimental
  class void extends TastyAnnotation: // error
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  object Bar:
    @experimental
    class void extends TastyAnnotation: // error
      def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

class Foo2:
  @experimental
  trait void extends TastyAnnotation // error

  object Bar:
    @experimental
    trait void extends TastyAnnotation // error

def test: Unit =
  @experimental
  class void extends TastyAnnotation: // error
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  trait void2 extends TastyAnnotation // error

  new TastyAnnotation {} // error

  ()

val test2: Unit =
  @experimental
  class void extends TastyAnnotation: // error
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  trait void2 extends TastyAnnotation // error

  new TastyAnnotation {} // error

  ()
