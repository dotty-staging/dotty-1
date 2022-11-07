import scala.annotation.{experimental, TastyAnnotation}
import scala.quoted._

@experimental
class hello extends TastyAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val helloSymbol = Symbol.newUniqueVal(tree.symbol.owner, "hello", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
    val helloVal = ValDef(helloSymbol, Some(Literal(StringConstant("Hello, World!"))))
    List(helloVal, tree)
}
