import scala.annotation.{experimental, TastyAnnotation}
import scala.quoted._

object Foo:
  @experimental
  class void extends TastyAnnotation:
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  object Bar:
    @experimental
    class void extends TastyAnnotation:
      def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)
