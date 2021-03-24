object Trees {
  class Type
  type Untyped = Null

  class Tree[-T >: Untyped]
  trait Ting[T >: Untyped <: Type] {
    type Tree = Trees.Tree[T]
    type RefTree = Trees.Tree[T]
  }
  object tpd extends Ting[Type]
}

object NotTrees {
  import Trees.tpd._
  def sanity[T](ts: List[T]) =
    ts match
      case _ : List[Int] => 0
      case _ => 1

  def foo(trees: List[Tree]) =
    trees match
      case (_ : RefTree) :: _ =>
        null
}
