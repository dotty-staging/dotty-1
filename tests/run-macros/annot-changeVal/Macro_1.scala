import scala.annotation.experimental
import scala.quoted.*
import scala.annotation.TastyAnnotation

object ChangeVal:
  @experimental
  class change(i: Int) extends TastyAnnotation {
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
      import quotes.reflect.*
      tree match
        case ValDef(n, t, _) => List(ValDef.copy(tree)(n, t, Some(Literal(IntConstant(i)))))
  }
