package dotty.tools.dotc
package transform

import core._
import Constants.Constant
import Contexts.Context
import Decorators._
import Flags._
import ast.Trees._
import Definitions._
import DenotTransformers._
import StdNames._
import Symbols._
import MegaPhase._
import Types._
import dotty.tools.dotc.ast.tpd

import scala.annotation.tailrec

/** TODO
 */
class GenericTuples extends MiniPhase with IdentityDenotTransformer {
  import tpd._

  def phaseName: String = "genericTuples"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = {
    if (tree.symbol == defn.DynamicTuple_dynamicCons) transformTupleCons(tree)
    else if (tree.symbol == defn.DynamicTuple_dynamicTail) transformTupleTail(tree)
    else if (tree.symbol == defn.DynamicTuple_dynamicSize) transformTupleSize(tree)
    else if (tree.symbol == defn.DynamicTuple_dynamicConcat) transformTupleConcat(tree)
    else if (tree.symbol == defn.DynamicTuple_dynamicApply) transformTupleApply(tree)
    else if (tree.symbol == defn.DynamicTuple_dynamicToArray) transformTupleToArray(tree)
    else super.transformApply(tree)
  }

  private def transformTupleCons(tree: tpd.Apply)(implicit ctx: Context): Tree = {
    val TypeApply(_, headType :: tailType :: Nil) = tree.fun
    val tail :: head :: Nil = tree.args
    tupleTypes(tree.tpe) match {
      case Some(tpes) =>
        val size = tpes.size
        if (size <= 5) {
          // val t = tail
          // TupleN+1(head, t._1, ..., t._n)
          evalOnce(Typed(tail, TypeTree(defn.tupleType(tpes.tail)))) { tup =>
            val elements = head :: (0 until size - 1).map(i => tup.select(nme.selectorName(i))).toList
            knownTupleFromElements(tpes, elements)
          }
        } else {
          // val it = Iterator.single(head) ++ tail.asInstanceOf[Product].productIterator
          // TupleN(it.next(), ..., it.next())
          val fullIterator = ref(defn.DynamicTuple_consIterator).appliedToArgs(head :: tail :: Nil)
          evalOnce(fullIterator) { it =>
            knownTupleFromIterator(tpes.length, it).asInstance(tree.tpe)
          }
        }
      case _ =>
        // DynamicTuple.dynamicCons:(tail, head)
        tree
    }
  }

  private def transformTupleTail(tree: tpd.Apply)(implicit ctx: Context): Tree = {
    val Apply(TypeApply(_, tpt :: Nil), tup :: Nil) = tree
    tupleTypes(tpt.tpe) match { // TODO tupleBoundedTypes
      case Some(tpes) =>
        val size = tpes.size
        assert(size > 0)
        if (size == 1) {
          // ()
          Literal(Constant(()))
        }
        else if (size <= 5) {
          // val t = tup.asInstanceOf[TupleN[...]]
          // TupleN-1(t._2, ..., t._n)
          evalOnce(Typed(tup, TypeTree(defn.tupleType(tpes)))) { tup =>
            val elements = (1 until size).map(i => tup.select(nme.selectorName(i))).toList
            knownTupleFromElements(tpes.tail, elements)
          }
        } else {
          // val it = this.asInstanceOf[Product].productIterator
          // it.next()
          // TupleN(it.next(), ..., it.next())
          evalOnce(tup.asInstance(defn.ProductType).select(nme.productIterator)) { it =>
            Block(
              it.select(nme.next).ensureApplied :: Nil,
              knownTupleFromIterator(size - 1, it).asInstance(tree.tpe)
            )
          }
        }
      case None =>
        // DynamicTuple.dynamicTail(tup)
        tree
    }
  }

  private def transformTupleSize(tree: tpd.Apply)(implicit ctx: Context): Tree = {
    tree.tpe.tryNormalize match {
      case tp: ConstantType => Literal(tp.value)
      case _ => tree
    }
  }

  private def transformTupleConcat(tree: tpd.Apply)(implicit ctx: Context): Tree = {
    val Apply(TypeApply(_, selfTp :: thatTp :: Nil), self :: that :: Nil) = tree

    (tupleTypes(selfTp.tpe), tupleTypes(that.tpe.widenTermRefExpr)) match {
      case (Some(tpes1), Some(tpes2)) =>
        val n = tpes1.size
        val m = tpes2.size
        if (n == 0) that
        else if (m == 0) self
        else if (n + m < 5) {
          // val t = self
          // val u = that
          // TupleN+M(t._1,..., t._N, u._1, ..., u._M)
          evalOnce(Typed(self, TypeTree(defn.tupleType(tpes1)))) { self =>
            evalOnce(Typed(that, TypeTree(defn.tupleType(tpes2)))) { that =>
              val types = tpes1 ::: tpes2
              val elements = {
                (0 until n).map(i => self.select(nme.selectorName(i))) ++
                (0 until m).map(i => that.select(nme.selectorName(i)))
              }.toList
              knownTupleFromElements(types, elements)
            }
          }
        } else {
          // val it = self.asInstanceOf[Product].productIterator ++ that.asInstanceOf[Product].productIterator
          // TupleN(it.next(), ..., it.next())
          val fullIterator = ref(defn.DynamicTuple_concatIterator).appliedToArgs(tree.args)
          evalOnce(fullIterator) { it =>
            knownTupleFromIterator(n + m, it).asInstance(tree.tpe)
          }
        }
      case _ =>
        // DynamicTuple.dynamicCons[This, that.type](self, that)
        tree
    }
  }

  private def transformTupleApply(tree: tpd.Apply)(implicit ctx: Context): Tree = {
    val Apply(TypeApply(_, tpt :: nTpt :: Nil), tup :: nTree :: Nil) = tree
    (tupleTypes(tpt.tpe), nTpt.tpe) match {
      case (Some(tpes), nTpe: ConstantType) =>
        val size = tpes.size
        val n = nTpe.value.intValue
        if (n < 0 || n >= size) {
          ctx.error("index out of bounds: " + n, nTree.underlyingArgument.sourcePos)
          tree
        } else if (size <= Definitions.MaxTupleArity) {
          // tup._n
          Typed(tup, TypeTree(defn.tupleType(tpes))).select(nme.selectorName(n))
        } else {
          // tup.asInstanceOf[TupleXXL].productElement(n)
          tup.asInstance(defn.TupleXXLType).select(nme.productElement).appliedTo(Literal(nTpe.value))
        }
      case (None, nTpe: ConstantType) if nTpe.value.intValue < 0 =>
        ctx.error("index out of bounds: " + nTpe.value.intValue, nTree.sourcePos)
        tree
      case _ =>
        // DynamicTuple.dynamicApply(tup, n)
        tree
    }
  }

  private def transformTupleToArray(tree: tpd.Apply)(implicit ctx: Context): Tree = {
    val Apply(_, tup :: Nil) = tree
    tupleTypes(tup.tpe.widen) match { // TODO tupleBoundedTypes
      case Some(tpes) =>
        val size = tpes.size
        if (size == 0) {
          // Array.emptyObjectArray
          ref(defn.ArrayModule.companionModule).select("emptyObjectArray".toTermName).ensureApplied
        } else if (size <= Definitions.MaxTupleArity) {
          // DynamicTuple.productToArray(tup.asInstanceOf[Product])
          ref(defn.DynamicTuple_productToArray).appliedTo(tup.asInstance(defn.ProductType))
        } else {
          // tup.asInstanceOf[TupleXXL].elems
          tup.asInstance(defn.TupleXXLType).select(nme.elems)
        }
      case None =>
        // DynamicTuple.dynamicToArray(tup)
        tree
    }
  }

  /** Create a TupleN (1 <= N < 23) from the elements */
  private def knownTupleFromElements(tpes: List[Type], elements: List[Tree])(implicit ctx: Context) = {
    val size = elements.size
    assert(0 < size && size <= Definitions.MaxTupleArity)
    val tupleModule = defn.TupleType(size).classSymbol.companionModule
    ref(tupleModule).select(nme.apply).appliedToTypes(tpes).appliedToArgs(elements)
  }

  private def knownTupleFromIterator(size: Int, it: Tree)(implicit ctx: Context): Tree = {
    if (size == 0) {
      // Unit for empty tuple
      Literal(Constant(())) // TODO should this code be here? Or assert(size > specializedSize)
    }
    else if (size <= Definitions.MaxTupleArity) {
      // TupleN(it.next(), ..., it.next())

      // TODO outline this code for the 22 alternatives (or less, may not need the smallest ones)?
      // This would yield smaller bytecode at the cost of an extra (easily JIT inlinable) call.
      // def dynamicTupleN(it: Iterator[Any]): TupleN[Any, ..., Any] = Tuple(it.next(), ..., it.next())
      val tpes = List.fill(size)(defn.AnyType)
      val elements = (0 until size).map(_ => it.select(nme.next)).toList
      knownTupleFromElements(tpes, elements)
    } else {
      // TupleXXL.fromIterator(it)
      ref(defn.TupleXXL_fromIterator).appliedTo(it)
    }
  }

  private def tupleTypes(tp: Type)(implicit ctx: Context): Option[List[Type]] = {
    @tailrec def rec(tp: Type, acc: List[Type]): Option[List[Type]] = tp match {
      case tp: AppliedType if defn.PairClass == tp.classSymbol => rec(tp.args(1), tp.args(0) :: acc)
      case tp: AppliedType if defn.isTupleClass(tp.tycon.classSymbol) => Some(acc.reverse ::: tp.args)
      case tp if tp.classSymbol == defn.UnitClass => Some(acc.reverse)
      case _ => None
    }
    rec(tp.stripTypeVar, Nil)
  }
}
