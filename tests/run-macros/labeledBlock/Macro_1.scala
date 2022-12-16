import scala.quoted.*
import util.chaining.scalaUtilChainingOps

object labeled:
  class Label[T] private[labeled]:
    def break(value: T): Nothing = throw new Break(this, value)
    def continue(): Nothing = throw new Continue(this)

  private def newLabel[T]: Label[T] = new Label[T]

  private final class Break(val label: Label[?], val value: Any) extends Throwable:
    override def fillInStackTrace(): Throwable = this

  private final class Continue(val label: Label[?]) extends Throwable:
    override def fillInStackTrace(): Throwable = this

  inline def block[T](inline body: Label[T] => T): T =
    ${ blockExpr[T]('body) }

  private def blockExpr[T: Type](body: Expr[Label[T] => T])(using Quotes): Expr[T] =
    val labelUse = analyzeLabel(body)
    import labelUse.{ localBreaks, localContinues, nonLocalBreaks, nonLocalContinues }

    def nonLocalBreakable(label: Expr[Label[T]])(using Quotes): Expr[T] =
      '{
        try ${
          if localContinues || nonLocalContinues then nonLocalContinuable(label)
          else '{ $body($label) }
        }
        catch
          case break: Break if break.label eq $label =>
            break.value.asInstanceOf[T]
      }

    def nonLocalContinuable(label: Expr[Label[T]])(using Quotes): Expr[T] =
      '{
        var res: T = null.asInstanceOf[T]
        var loop = true
        while loop do
          try
            res = $body($label)
            loop = false
          catch
            case continue: Continue if continue.label eq $label =>
        res
      }

    // TODO optimize local continues and breaks
    '{
      val label = newLabel[T]
      ${
        if localBreaks || nonLocalBreaks then nonLocalBreakable('label)
        else if localContinues || nonLocalContinues then nonLocalContinuable('label)
        else '{ $body(label) }
      }
    }
    // .tap(expr => println(expr.show))


  private class LabelUse(val localBreaks: Boolean, val localContinues: Boolean, val nonLocalBreaks: Boolean, val nonLocalContinues: Boolean)

  private def analyzeLabel[T: Type](body: Expr[Label[T] => T])(using Quotes): LabelUse =
    import quotes.reflect.*

    var localBreaks = false
    var localContinues = false
    var nonLocalBreaks = false
    var nonLocalContinues = false

    class RhsTraverser(labelSym: Symbol) extends TreeTraverser:
      private var local = true

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
        case _: DefDef | _: ClassDef =>
          val old = local
          local = false
          try super.traverseTree(tree)(owner)
          finally local = old
        case Apply(Select(label: Ident, "break"), List(value)) if label.symbol == labelSym =>
          if local then localBreaks = true
          else nonLocalBreaks = true
        case Apply(Select(label: Ident, "continue"), Nil) if label.symbol == labelSym =>
          if local then localContinues = true
          else nonLocalContinues = true
        case Apply(_, args) =>
          for
            case label @ Ident(_) <- args
            if label.symbol == labelSym
          do
            // label escapes known code
            nonLocalBreaks = true
            nonLocalContinues = true
          super.traverseTree(tree)(owner)
        case _ =>
          super.traverseTree(tree)(owner)

    object BodyTreeTraverser extends TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
        case tree @ DefDef("$anonfun", List(TermParamClause(List(label))), _, Some(rhs)) =>
          val rhsTreeTraverser = new RhsTraverser(label.symbol)
          rhsTreeTraverser.traverseTree(rhs)(tree.symbol)
        case tree =>
          super.traverseTree(tree)(owner)

    BodyTreeTraverser.traverseTree(body.asTerm)(Symbol.spliceOwner)

    new LabelUse(localBreaks, localContinues, nonLocalBreaks, nonLocalContinues)
