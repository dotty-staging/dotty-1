import scala.annotation.{experimental, TastyAnnotation}
import scala.quoted._

@experimental
class error extends TastyAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    quotes.reflect.report.error("MACRO ERROR", tree.pos)
    List(tree)
}
