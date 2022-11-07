import scala.annotation.{experimental, TastyAnnotation}
import scala.quoted._

@experimental
class nilAnnot extends TastyAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    Nil
}
