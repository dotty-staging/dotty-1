import scala.annotation.{experimental, TastyAnnotation}
import scala.quoted._

@experimental
class void extends TastyAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    List(tree)
