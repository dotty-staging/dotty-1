import scala.annotation.TastyAnnotation
import scala.quoted.*

class annot extends TastyAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    List(tree)

def f(using Quotes) = '{
  @annot def g = 4 // error
  g
}

