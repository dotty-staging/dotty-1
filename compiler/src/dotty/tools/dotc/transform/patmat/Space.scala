package dotty.tools.dotc
package transform
package patmat

import core._
import Types._
import Contexts._
import Flags._
import ast._
import Trees._
import Decorators._
import Symbols._
import StdNames._
import NameOps._
import Constants._
import typer._
import Applications._
import Inferencing._
import ProtoTypes._
import transform.SymUtils._
import reporting._
import config.Printers.{exhaustivity => debug}
import util.SrcPos
import NullOpsDecorator._
import collection.mutable

/** Space logic for checking exhaustivity and unreachability of pattern matching
 *
 *  Space can be thought of as a set of possible values. A type or a pattern
 *  both refer to spaces. The space of a type is the values that inhabit the
 *  type. The space of a pattern is the values that can be covered by the
 *  pattern.
 *
 *  Space is recursively defined as follows:
 *
 *      1. `Empty` is a space
 *      2. For a type T, `Typ(T)` is a space
 *      3. A union of spaces `S1 | S2 | ...` is a space
 *      4. `Prod(S1, S2, ..., Sn)` is a product space.
 *
 *  For the problem of exhaustivity check, its formulation in terms of space is as follows:
 *
 *      Is the space Typ(T) a subspace of the union of space covered by all the patterns?
 *
 *  The problem of unreachable patterns can be formulated as follows:
 *
 *      Is the space covered by a pattern a subspace of the space covered by previous patterns?
 *
 *  Assumption:
 *    (1) One case class cannot be inherited directly or indirectly by another
 *        case class.
 *    (2) Inheritance of a case class cannot be well handled by the algorithm.
 *
 */


/** space definition */
sealed trait Space

/** Empty space */
case object Empty extends Space

/** Space representing the set of all values of a type
 *
 * @param tp: the type this space represents
 * @param decomposed: does the space result from decomposition? Used for pretty print
 *
 */
case class Typ(tp: Type, decomposed: Boolean = true) extends Space

/** Space representing an extractor pattern */
case class Prod(tp: Type, unappTp: TermRef, params: List[Space], full: Boolean) extends Space

/** Union of spaces */
case class Or(spaces: List[Space]) extends Space

/** abstract space logic */
trait SpaceLogic {
  /** Is `tp1` a subtype of `tp2`? */
  def isSubType(tp1: Type, tp2: Type): Boolean

  /** True if we can assume that the two unapply methods are the same.
   *  That is, given the same parameter, they return the same result.
   *
   *  We assume that unapply methods are pure, but the same method may
   *  be called with different prefixes, thus behaving differently.
   */
  def isSameUnapply(tp1: TermRef, tp2: TermRef): Boolean

  /** Return a space containing the values of both types.
   *
   * The types should be atomic (non-decomposable) and unrelated (neither
   * should be a subtype of the other).
   */
  def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Space

  /** Is the type `tp` decomposable? i.e. all values of the type can be covered
   *  by its decomposed types.
   *
   * Abstract sealed class, OrType, Boolean and Java enums can be decomposed.
   */
  def canDecompose(tp: Type): Boolean

  /** Return term parameter types of the extractor `unapp` */
  def signature(unapp: TermRef, scrutineeTp: Type, argLen: Int): List[Type]

  /** Get components of decomposable types */
  def decompose(tp: Type): List[Space]

  /** Display space in string format */
  def show(sp: Space): String

  /** Simplify space using the laws, there's no nested union after simplify
   *
   *  @param aggressive if true and OR space has less than 5 components, `simplify` will
   *                    collapse `sp1 | sp2` to `sp1` if `sp2` is a subspace of `sp1`.
   *
   *                    This reduces noise in counterexamples.
   */
  def simplify(space: Space, aggressive: Boolean = false)(using Context): Space = trace(s"simplify ${show(space)}, aggressive = $aggressive --> ", debug, x => show(x.asInstanceOf[Space]))(space match {
    case Prod(tp, fun, spaces, full) =>
      val sp = Prod(tp, fun, spaces.map(simplify(_)), full)
      if (sp.params.contains(Empty)) Empty
      else if (canDecompose(tp) && decompose(tp).isEmpty) Empty
      else sp
    case Or(spaces) =>
      val buf = new mutable.ListBuffer[Space]
      def include(s: Space) = if s != Empty then buf += s
      for space <- spaces do
        simplify(space) match
          case Or(ss) => ss.foreach(include)
          case s => include(s)
      val set = buf.toList

      if (set.isEmpty) Empty
      else if (set.size == 1) set.toList(0)
      else if (aggressive && spaces.size < 5) {
        val res = set.map(sp => (sp, set.filter(_ ne sp))).find {
          case (sp, sps) =>
            isSubspace(sp, Or(sps))
        }
        if (res.isEmpty) Or(set)
        else simplify(Or(res.get._2), aggressive)
      }
      else Or(set)
    case Typ(tp, _) =>
      if (canDecompose(tp) && decompose(tp).isEmpty) Empty
      else space
    case _ => space
  })

  /** Flatten space to get rid of `Or` for pretty print */
  def flatten(space: Space)(using Context): Seq[Space] = space match {
    case Prod(tp, fun, spaces, full) =>
      val ss = LazyList(spaces: _*).map(flatten)

      ss.foldLeft(LazyList(Nil : List[Space])) { (acc, flat) =>
        for { sps <- acc; s <- flat }
        yield sps :+ s
      }.map { sps =>
        Prod(tp, fun, sps, full)
      }

    case Or(spaces) =>
      LazyList(spaces: _*).flatMap(flatten)

    case _ =>
      List(space)
  }

  /** Is `a` a subspace of `b`? Equivalent to `a - b == Empty`, but faster */
  def isSubspace(a: Space, b: Space)(using Context): Boolean = trace(s"${show(a)} < ${show(b)}", debug) {
    def tryDecompose1(tp: Type) = canDecompose(tp) && isSubspace(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = canDecompose(tp) && isSubspace(a, Or(decompose(tp)))

    (simplify(a), simplify(b)) match {
      case (Empty, _) => true
      case (_, Empty) => false
      case (Or(ss), _) =>
        ss.forall(isSubspace(_, b))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (Typ(tp1, _), Or(ss)) =>  // optimization: don't go to subtraction too early
        ss.exists(isSubspace(a, _)) || tryDecompose1(tp1)
      case (_, Or(_)) =>
        simplify(minus(a, b)) == Empty
      case (Prod(tp1, _, _, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (Typ(tp1, _), Prod(tp2, fun, ss, full)) =>
        // approximation: a type can never be fully matched by a partial extractor
        full && isSubType(tp1, tp2) && isSubspace(Prod(tp2, fun, signature(fun, tp2, ss.length).map(Typ(_, false)), full), b)
      case (Prod(_, fun1, ss1, _), Prod(_, fun2, ss2, _)) =>
        isSameUnapply(fun1, fun2) && ss1.zip(ss2).forall((isSubspace _).tupled)
    }
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space)(using Context): Space = trace(s"${show(a)} & ${show(b)}", debug, x => show(x.asInstanceOf[Space])) {
    def tryDecompose1(tp: Type) = intersect(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = intersect(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) | (_, Empty) => Empty
      case (_, Or(ss)) => Or(ss.map(intersect(a, _)).filterConserve(_ ne Empty))
      case (Or(ss), _) => Or(ss.map(intersect(_, b)).filterConserve(_ ne Empty))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else intersectUnrelatedAtomicTypes(tp1, tp2)
      case (Typ(tp1, _), Prod(tp2, fun, ss, true)) =>
        if (isSubType(tp2, tp1)) b
        else if (isSubType(tp1, tp2)) a // problematic corner case: inheriting a case class
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else Empty
      case (Typ(tp1, _), Prod(tp2, _, _, false)) =>
        if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) b  // prefer extractor space for better approximation
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else Empty
      case (Prod(tp1, fun, ss, true), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (isSubType(tp2, tp1)) a  // problematic corner case: inheriting a case class
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Prod(tp1, _, _, false), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) a
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Prod(tp1, fun1, ss1, full), Prod(tp2, fun2, ss2, _)) =>
        if (!isSameUnapply(fun1, fun2)) Empty
        else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) Empty
        else Prod(tp1, fun1, ss1.zip(ss2).map((intersect _).tupled), full)
    }
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space)(using Context): Space = trace(s"${show(a)} - ${show(b)}", debug, x => show(x.asInstanceOf[Space])) {
    def tryDecompose1(tp: Type) = minus(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = minus(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) => Empty
      case (_, Empty) => a
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Typ(tp1, _), Prod(tp2, fun, ss, true)) =>
        // rationale: every instance of `tp1` is covered by `tp2(_)`
        if (isSubType(tp1, tp2)) minus(Prod(tp1, fun, signature(fun, tp1, ss.length).map(Typ(_, false)), true), b)
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else a
      case (_, Or(ss)) =>
        ss.foldLeft(a)(minus)
      case (Or(ss), _) =>
        Or(ss.map(minus(_, b)))
      case (Prod(tp1, fun, ss, true), Typ(tp2, _)) =>
        // uncovered corner case: tp2 :< tp1
        if (isSubType(tp1, tp2)) Empty
        else if (simplify(a) == Empty) Empty
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Prod(tp1, _, _, false), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else a
      case (Typ(tp1, _), Prod(tp2, _, _, false)) =>
        a  // approximation
      case (Prod(tp1, fun1, ss1, full), Prod(tp2, fun2, ss2, _)) =>
        if (!isSameUnapply(fun1, fun2)) return a

        val range = (0 until ss1.size).toList
        val cache = Array.fill[Space](ss2.length)(null)
        def sub(i: Int) =
          if cache(i) == null then
            cache(i) = minus(ss1(i), ss2(i))
          cache(i)
        end sub

        if range.exists(i => isSubspace(ss1(i), sub(i))) then a
        else if cache.forall(sub => isSubspace(sub, Empty)) then Empty
        else
          // `(_, _, _) - (Some, None, _)` becomes `(None, _, _) | (_, Some, _) | (_, _, Empty)`
          Or(range.map { i => Prod(tp1, fun1, ss1.updated(i, sub(i)), full) })
    }
  }
}

object SpaceEngine {

  /** Is the unapply irrefutable?
   *  @param  unapp   The unapply function reference
   */
  def isIrrefutableUnapply(unapp: tpd.Tree, patSize: Int)(using Context): Boolean = {
    val unappResult = unapp.tpe.widen.finalResultType
    unappResult.isRef(defn.SomeClass) ||
    unappResult <:< ConstantType(Constant(true)) ||
    (unapp.symbol.is(Synthetic) && unapp.symbol.owner.linkedClass.is(Case)) ||  // scala2 compatibility
    (patSize != -1 && productArity(unappResult) == patSize) || {
      val isEmptyTp = extractorMemberType(unappResult, nme.isEmpty, unapp.srcPos)
      isEmptyTp <:< ConstantType(Constant(false))
    }
  }

  /** Is the unapplySeq irrefutable?
   *  @param  unapp   The unapplySeq function reference
   */
  def isIrrefutableUnapplySeq(unapp: tpd.Tree, patSize: Int)(using Context): Boolean = {
    val unappResult = unapp.tpe.widen.finalResultType
    unappResult.isRef(defn.SomeClass) ||
    (unapp.symbol.is(Synthetic) && unapp.symbol.owner.linkedClass.is(Case)) ||  // scala2 compatibility
    unapplySeqTypeElemTp(unappResult).exists ||
    isProductSeqMatch(unappResult, patSize) ||
    {
      val isEmptyTp = extractorMemberType(unappResult, nme.isEmpty, unapp.srcPos)
      isEmptyTp <:< ConstantType(Constant(false))
    }
  }
}

/** Scala implementation of space logic */
class SpaceEngine(using Context) extends SpaceLogic {
  import tpd._
  import SpaceEngine._

  private val scalaSeqFactoryClass = defn.SeqFactoryClass
  private val scalaListType        = defn.ListClass.typeRef
  private val scalaNilType         = defn.NilModule.termRef
  private val scalaConsType        = defn.ConsClass.typeRef

  private val constantNullType     = ConstantType(Constant(null))
  private val constantNullSpace    = Typ(constantNullType)

  /** Does the given tree stand for the literal `null`? */
  def isNullLit(tree: Tree): Boolean = tree match {
    case Literal(Constant(null)) => true
    case _ => false
  }

  /** Does the given space contain just the value `null`? */
  def isNullSpace(space: Space): Boolean = space match {
    case Typ(tpe, _) => tpe.dealias == constantNullType || tpe.isNullType
    case Or(spaces) => spaces.forall(isNullSpace)
    case _ => false
  }

  override def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Space = trace(s"atomic intersection: ${AndType(tp1, tp2).show}", debug) {
    // Precondition: !isSubType(tp1, tp2) && !isSubType(tp2, tp1).
    if (!ctx.explicitNulls && (tp1.isNullType || tp2.isNullType)) {
      // Since projections of types don't include null, intersection with null is empty.
      Empty
    }
    else {
      val res = TypeComparer.provablyDisjoint(tp1, tp2)

      if (res) Empty
      else if (tp1.isSingleton) Typ(tp1, true)
      else if (tp2.isSingleton) Typ(tp2, true)
      else Typ(AndType(tp1, tp2), true)
    }
  }

  /** Return the space that represents the pattern `pat` */
  def project(pat: Tree): Space = pat match {
    case Literal(c) =>
      if (c.value.isInstanceOf[Symbol])
        Typ(c.value.asInstanceOf[Symbol].termRef, false)
      else
        Typ(ConstantType(c), false)

    case pat: Ident if isBackquoted(pat) =>
      Typ(pat.tpe, false)

    case Ident(nme.WILDCARD) =>
      Or(Typ(erase(pat.tpe.stripAnnots), false) :: constantNullSpace :: Nil)

    case Ident(_) | Select(_, _) =>
      Typ(erase(pat.tpe.stripAnnots), false)

    case Alternative(trees) =>
      Or(trees.map(project(_)))

    case Bind(_, pat) =>
      project(pat)

    case SeqLiteral(pats, _) =>
      projectSeq(pats)

    case UnApply(fun, _, pats) =>
      val (fun1, _, _) = decomposeCall(fun)
      val funRef = fun1.tpe.asInstanceOf[TermRef]
      if (fun.symbol.name == nme.unapplySeq)
        if (fun.symbol.owner == scalaSeqFactoryClass)
          projectSeq(pats)
        else {
          val (arity, elemTp, resultTp) = unapplySeqInfo(fun.tpe.widen.finalResultType, fun.srcPos)
          if (elemTp.exists)
            Prod(erase(pat.tpe.stripAnnots), funRef, projectSeq(pats) :: Nil, isIrrefutableUnapplySeq(fun, pats.size))
          else
            Prod(erase(pat.tpe.stripAnnots), funRef, pats.take(arity - 1).map(project) :+ projectSeq(pats.drop(arity - 1)), isIrrefutableUnapplySeq(fun, pats.size))
        }
      else
        Prod(erase(pat.tpe.stripAnnots), funRef, pats.map(project), isIrrefutableUnapply(fun, pats.length))

    case Typed(pat @ UnApply(_, _, _), _) =>
      project(pat)

    case Typed(expr, _) =>
      Typ(erase(expr.tpe.stripAnnots), true)

    case This(_) =>
      Typ(pat.tpe.stripAnnots, false)

    case EmptyTree =>         // default rethrow clause of try/catch, check tests/patmat/try2.scala
      Typ(WildcardType, false)

    case Block(Nil, expr) =>
      project(expr)

    case _ =>
      // Pattern is an arbitrary expression; assume a skolem (i.e. an unknown value) of the pattern type
      Typ(pat.tpe.narrow, false)
  }

  private def project(tp: Type): Space = tp match {
    case OrType(tp1, tp2) => Or(project(tp1) :: project(tp2) :: Nil)
    case tp => Typ(tp, decomposed = true)
  }

  private def unapplySeqInfo(resTp: Type, pos: SrcPos)(using Context): (Int, Type, Type) = {
    var resultTp = resTp
    var elemTp = unapplySeqTypeElemTp(resultTp)
    var arity = productArity(resultTp, pos)
    if (!elemTp.exists && arity <= 0) {
      resultTp = resTp.select(nme.get).finalResultType
      elemTp = unapplySeqTypeElemTp(resultTp.widen)
      arity = productSelectorTypes(resultTp, pos).size
    }
    (arity, elemTp, resultTp)
  }

  /** Erase pattern bound types with WildcardType
   *
   *  For example, the type `C[T$1]` should match any `C[?]`, thus
   *  `v` should be `WildcardType` instead of `T$1`:
   *
   *     sealed trait B
   *     case class C[T](v: T) extends B
   *     (b: B) match {
   *        case C(v) =>      //    case C.unapply[T$1 @ T$1](v @ _):C[T$1]
   *     }
   *
   *  However, we cannot use WildcardType for Array[?], due to that
   *  `Array[WildcardType] <: Array[Array[WildcardType]]`, which may
   *  cause false unreachable warnings. See tests/patmat/t2425.scala
   *
   *  We cannot use type erasure here, as it would lose the constraints
   *  involving GADTs. For example, in the following code, type
   *  erasure would loose the constraint that `x` and `y` must be
   *  the same type, resulting in false inexhaustive warnings:
   *
   *     sealed trait Expr[T]
   *     case class IntExpr(x: Int) extends Expr[Int]
   *     case class BooleanExpr(b: Boolean) extends Expr[Boolean]
   *
   *     def foo[T](x: Expr[T], y: Expr[T]) = (x, y) match {
   *       case (IntExpr(_), IntExpr(_)) =>
   *       case (BooleanExpr(_), BooleanExpr(_)) =>
   *     }
   */
  private def erase(tp: Type, inArray: Boolean = false): Type = trace(i"$tp erased to", debug) {

    tp match {
      case tp @ AppliedType(tycon, args) =>
        if tycon.typeSymbol.isPatternBound then return WildcardType

        val args2 =
          if (tycon.isRef(defn.ArrayClass)) args.map(arg => erase(arg, inArray = true))
          else args.map(arg => erase(arg, inArray = false))
        tp.derivedAppliedType(erase(tycon, inArray), args2)

      case tp @ OrType(tp1, tp2) =>
        OrType(erase(tp1, inArray), erase(tp2, inArray), tp.isSoft)

      case AndType(tp1, tp2) =>
        AndType(erase(tp1, inArray), erase(tp2, inArray))

      case tp @ RefinedType(parent, _, _) =>
        erase(parent)

      case tref: TypeRef if tref.symbol.isPatternBound =>
        if (inArray) tref.underlying else WildcardType

      case _ => tp
    }
  }

  /** Space of the pattern: unapplySeq(a, b, c: _*)
   */
  def projectSeq(pats: List[Tree]): Space = {
    if (pats.isEmpty) return Typ(scalaNilType, false)

    val (items, zero) = if (isWildcardStarArg(pats.last))
      (pats.init, Typ(scalaListType.appliedTo(pats.last.tpe.elemType), false))
    else
      (pats, Typ(scalaNilType, false))

    val unapplyTp = scalaConsType.classSymbol.companionModule.termRef.select(nme.unapply)
    items.foldRight[Space](zero) { (pat, acc) =>
      val consTp = scalaConsType.appliedTo(pats.head.tpe.widen)
      Prod(consTp, unapplyTp, project(pat) :: acc :: Nil, true)
    }
  }

  /** Is `tp1` a subtype of `tp2`?  */
  def isSubType(tp1: Type, tp2: Type): Boolean = {
    debug.println(TypeComparer.explained(_.isSubType(tp1, tp2)))
    val res = if (ctx.explicitNulls) {
      tp1 <:< tp2
    } else {
      (tp1 != constantNullType || tp2 == constantNullType) && tp1 <:< tp2
    }
    res
  }

  def isSameUnapply(tp1: TermRef, tp2: TermRef): Boolean =
    tp1.prefix.isStable && tp2.prefix.isStable && tp1 =:= tp2

  /** Parameter types of the case class type `tp`. Adapted from `unapplyPlan` in patternMatcher  */
  def signature(unapp: TermRef, scrutineeTp: Type, argLen: Int): List[Type] = {
    val unappSym = unapp.symbol

    // println("scrutineeTp = " + scrutineeTp.show)

    val mt: MethodType = unapp.widen match {
      case mt: MethodType => mt
      case pt: PolyType   =>
        inContext(ctx.fresh.setExploreTyperState()) {
          val tvars = pt.paramInfos.map(newTypeVar)
          val mt = pt.instantiate(tvars).asInstanceOf[MethodType]
          scrutineeTp <:< mt.paramInfos(0)
          // force type inference to infer a narrower type: could be singleton
          // see tests/patmat/i4227.scala
          mt.paramInfos(0) <:< scrutineeTp
          isFullyDefined(mt, ForceDegree.all)
          mt
        }
    }

    // Case unapply:
    // 1. return types of constructor fields if the extractor is synthesized for Scala2 case classes & length match
    // 2. return Nil if unapply returns Boolean  (boolean pattern)
    // 3. return product selector types if unapply returns a product type (product pattern)
    // 4. return product selectors of `T` where `def get: T` is a member of the return type of unapply & length match (named-based pattern)
    // 5. otherwise, return `T` where `def get: T` is a member of the return type of unapply
    //
    // Case unapplySeq:
    // 1. return the type `List[T]` where `T` is the element type of the unapplySeq return type `Seq[T]`

    val resTp = mt.finalResultType

    val sig =
      if (resTp.isRef(defn.BooleanClass))
        List()
      else {
        val isUnapplySeq = unappSym.name == nme.unapplySeq

        if (isUnapplySeq) {
          val (arity, elemTp, resultTp) = unapplySeqInfo(resTp, unappSym.srcPos)
          if (elemTp.exists) scalaListType.appliedTo(elemTp) :: Nil
          else {
            val sels = productSeqSelectors(resultTp, arity, unappSym.srcPos)
            sels.init :+ scalaListType.appliedTo(sels.last)
          }
        }
        else {
          val arity = productArity(resTp, unappSym.srcPos)
          if (arity > 0)
            productSelectorTypes(resTp, unappSym.srcPos)
          else {
            val getTp = resTp.select(nme.get).finalResultType.widenTermRefExpr
            if (argLen == 1) getTp :: Nil
            else productSelectorTypes(getTp, unappSym.srcPos)
          }
        }
      }

    debug.println(s"signature of ${unappSym.showFullName} ----> ${sig.map(_.show).mkString(", ")}")

    sig.map(_.annotatedToRepeated)
  }

  /** Decompose a type into subspaces -- assume the type can be decomposed */
  def decompose(tp: Type): List[Space] =
    tp.dealias match {
      case AndType(tp1, tp2) =>
        intersect(Typ(tp1, false), Typ(tp2, false)) match {
          case Or(spaces) => spaces
          case Empty => Nil
          case space => List(space)
        }
      case OrType(tp1, tp2) => List(Typ(tp1, true), Typ(tp2, true))
      case tp if tp.isRef(defn.BooleanClass) =>
        List(
          Typ(ConstantType(Constant(true)), true),
          Typ(ConstantType(Constant(false)), true)
        )
      case tp if tp.isRef(defn.UnitClass) =>
        Typ(ConstantType(Constant(())), true) :: Nil
      case tp if tp.classSymbol.isAllOf(JavaEnumTrait) =>
        tp.classSymbol.children.map(sym => Typ(sym.termRef, true))
      case tp =>
        val children = tp.classSymbol.children
        debug.println(s"candidates for ${tp.show} : [${children.map(_.show).mkString(", ")}]")

        val parts = children.map { sym =>
          val sym1 = if (sym.is(ModuleClass)) sym.sourceModule else sym
          val refined = TypeOps.refineUsingParent(tp, sym1)

          debug.println(sym1.show + " refined to " + refined.show)

          def inhabited(tp: Type): Boolean =
            tp.dealias match {
              case AndType(tp1, tp2) => !TypeComparer.provablyDisjoint(tp1, tp2)
              case OrType(tp1, tp2) => inhabited(tp1) || inhabited(tp2)
              case tp: RefinedType => inhabited(tp.parent)
              case tp: TypeRef => inhabited(tp.prefix)
              case _ => true
            }

          if (inhabited(refined)) refined
          else NoType
        } filter(_.exists)

        debug.println(s"${tp.show} decomposes to [${parts.map(_.show).mkString(", ")}]")

        parts.map(Typ(_, true))
    }


  /** Abstract sealed types, or-types, Boolean and Java enums can be decomposed */
  def canDecompose(tp: Type): Boolean =
    val res = tp.dealias match
      case _: SingletonType => false
      case _: OrType => true
      case and: AndType => canDecompose(and.tp1) || canDecompose(and.tp2)
      case _ =>
        val cls = tp.classSymbol
        cls.is(Sealed)
        && cls.isOneOf(AbstractOrTrait)
        && !cls.hasAnonymousChild
        && cls.children.nonEmpty
        || cls.isAllOf(JavaEnumTrait)
        || tp.isRef(defn.BooleanClass)
        || tp.isRef(defn.UnitClass)
    debug.println(s"decomposable: ${tp.show} = $res")
    res

  /** Show friendly type name with current scope in mind
   *
   *  E.g.    C.this.B     -->  B     if current owner is C
   *          C.this.x.T   -->  x.T   if current owner is C
   *          X[T]         -->  X
   *          C            -->  C     if current owner is C !!!
   *
   */
  def showType(tp: Type, showTypeArgs: Boolean = false): String = {
    val enclosingCls = ctx.owner.enclosingClass

    def isOmittable(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName ||
        ctx.definitions.unqualifiedOwnerTypes.exists(_.symbol == sym) ||
        sym.showFullName.startsWith("scala.") ||
        sym == enclosingCls || sym == enclosingCls.sourceModule

    def refinePrefix(tp: Type): String = tp match {
      case NoPrefix => ""
      case tp: NamedType if isOmittable(tp.symbol) => ""
      case tp: ThisType => refinePrefix(tp.tref)
      case tp: RefinedType => refinePrefix(tp.parent)
      case tp: NamedType => tp.name.show.stripSuffix("$")
      case tp: TypeVar => refinePrefix(tp.instanceOpt)
      case _ => tp.show
    }

    def refine(tp: Type): String = tp.stripped match {
      case tp: RefinedType => refine(tp.parent)
      case tp: AppliedType =>
        refine(tp.typeConstructor) + (
          if (showTypeArgs) tp.argInfos.map(refine).mkString("[", ",", "]")
          else ""
        )
      case tp: ThisType => refine(tp.tref)
      case tp: NamedType =>
        val pre = refinePrefix(tp.prefix)
        if (tp.name == tpnme.higherKinds) pre
        else if (pre.isEmpty) tp.name.show.stripSuffix("$")
        else pre + "." + tp.name.show.stripSuffix("$")
      case tp: OrType => refine(tp.tp1) + " | " + refine(tp.tp2)
      case _: TypeBounds => "_"
      case _ => tp.show.stripSuffix("$")
    }

    refine(tp)
  }

  /** Whether the counterexample is satisfiable. The space is flattened and non-empty. */
  def satisfiable(sp: Space): Boolean = {
    def impossible: Nothing = throw new AssertionError("`satisfiable` only accepts flattened space.")

    def genConstraint(space: Space): List[(Type, Type)] = space match {
      case Prod(tp, unappTp, ss, _) =>
        val tps = signature(unappTp, tp, ss.length)
        ss.zip(tps).flatMap {
          case (sp : Prod, tp) => sp.tp -> tp :: genConstraint(sp)
          case (Typ(tp1, _), tp2) => tp1 -> tp2 :: Nil
          case _ => impossible
        }
      case Typ(_, _) => Nil
      case _ => impossible
    }

    def checkConstraint(constrs: List[(Type, Type)])(using Context): Boolean = {
      val tvarMap = collection.mutable.Map.empty[Symbol, TypeVar]
      val typeParamMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tref: TypeRef if tref.symbol.is(TypeParam) =>
            tvarMap.getOrElseUpdate(tref.symbol, newTypeVar(tref.underlying.bounds))
          case tp => mapOver(tp)
        }
      }

      constrs.forall { case (tp1, tp2) => typeParamMap(tp1) <:< typeParamMap(tp2) }
    }

    checkConstraint(genConstraint(sp))(using ctx.fresh.setNewTyperState())
  }

  def show(ss: Seq[Space]): String = ss.map(show).mkString(", ")
  /** Display spaces */
  def show(s: Space): String = {
    def params(tp: Type): List[Type] = tp.classSymbol.primaryConstructor.info.firstParamTypes

    /** does the companion object of the given symbol have custom unapply */
    def hasCustomUnapply(sym: Symbol): Boolean = {
      val companion = sym.companionModule
      companion.findMember(nme.unapply, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists ||
        companion.findMember(nme.unapplySeq, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists
    }

    def doShow(s: Space, flattenList: Boolean = false): String = s match {
      case Empty => "empty"
      case Typ(c: ConstantType, _) => "" + c.value.value
      case Typ(tp: TermRef, _) =>
        if (flattenList && tp <:< scalaNilType) ""
        else tp.symbol.showName
      case Typ(tp, decomposed) =>

        val sym = tp.classSymbol

        if (ctx.definitions.isTupleType(tp))
          params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (scalaListType.isRef(sym))
          if (flattenList) "_: _*" else "_: List"
        else if (scalaConsType.isRef(sym))
          if (flattenList) "_, _: _*"  else "List(_, _: _*)"
        else if (tp.classSymbol.is(Sealed) && tp.classSymbol.hasAnonymousChild)
          "_: " + showType(tp) + " (anonymous)"
        else if (tp.classSymbol.is(CaseClass) && !hasCustomUnapply(tp.classSymbol))
        // use constructor syntax for case class
          showType(tp) + params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (decomposed) "_: " + showType(tp, showTypeArgs = true)
        else "_"
      case Prod(tp, fun, params, _) =>
        if (ctx.definitions.isTupleType(tp))
          "(" + params.map(doShow(_)).mkString(", ") + ")"
        else if (tp.isRef(scalaConsType.symbol))
          if (flattenList) params.map(doShow(_, flattenList)).mkString(", ")
          else params.map(doShow(_, flattenList = true)).filter(!_.isEmpty).mkString("List(", ", ", ")")
        else {
          val sym = fun.symbol
          val isUnapplySeq = sym.name.eq(nme.unapplySeq)
          val paramsStr = params.map(doShow(_, flattenList = isUnapplySeq)).mkString("(", ", ", ")")
          showType(fun.prefix) + paramsStr
        }
      case Or(ss) =>
        ss.map(doShow(_, flattenList)).mkString(" | ")
    }

    doShow(s, flattenList = false)
  }

  private def exhaustivityCheckable(sel: Tree): Boolean = {
    // Possible to check everything, but be compatible with scalac by default
    def isCheckable(tp: Type): Boolean =
      !tp.hasAnnotation(defn.UncheckedAnnot) && {
        val tpw = tp.widen.dealias
        ctx.settings.YcheckAllPatmat.value ||
        tpw.typeSymbol.is(Sealed) ||
        tpw.isInstanceOf[OrType] ||
        (tpw.isInstanceOf[AndType] && {
          val and = tpw.asInstanceOf[AndType]
          isCheckable(and.tp1) || isCheckable(and.tp2)
        }) ||
        tpw.isRef(defn.BooleanClass) ||
        tpw.typeSymbol.isAllOf(JavaEnumTrait) ||
        (defn.isTupleType(tpw) && tpw.argInfos.exists(isCheckable(_)))
      }

    val res = isCheckable(sel.tpe)
    debug.println(s"exhaustivity checkable: ${sel.show} = $res")
    res
  }

  /** Whehter counter-examples should be further checked? True for GADTs. */
  private def shouldCheckExamples(tp: Type): Boolean =
    new TypeAccumulator[Boolean] {
      override def apply(b: Boolean, tp: Type): Boolean = tp match {
        case tref: TypeRef if tref.symbol.is(TypeParam) && variance != 1 => true
        case tp => b || foldOver(b, tp)
      }
    }.apply(false, tp)

  def checkExhaustivity(_match: Match): Unit = {
    val Match(sel, cases) = _match
    val selTyp = sel.tpe.widen.dealias

    if (!exhaustivityCheckable(sel)) return

    val patternSpace = Or(cases.foldLeft(List.empty[Space]) { (acc, x) =>
      val space = if (x.guard.isEmpty) project(x.pat) else Empty
      debug.println(s"${x.pat.show} ====> ${show(space)}")
      space :: acc
    })

    val checkGADTSAT = shouldCheckExamples(selTyp)

    val uncovered =
      flatten(simplify(minus(project(selTyp), patternSpace), aggressive = true)).filter { s =>
        s != Empty && (!checkGADTSAT || satisfiable(s))
      }


    if uncovered.nonEmpty then
      val hasMore = uncovered.lengthCompare(6) > 0
      report.warning(PatternMatchExhaustivity(show(uncovered.take(6)), hasMore), sel.srcPos)
  }

  private def redundancyCheckable(sel: Tree): Boolean =
    // Ignore Expr[T] and Type[T] for unreachability as a special case.
    // Quote patterns produce repeated calls to the same unapply method, but with different implicit parameters.
    // Since we assume that repeated calls to the same unapply method overlap
    // and implicit parameters cannot normally differ between two patterns in one `match`,
    // the easiest solution is just to ignore Expr[T] and Type[T].
    !sel.tpe.hasAnnotation(defn.UncheckedAnnot)
    && !sel.tpe.widen.isRef(defn.QuotedExprClass)
    && !sel.tpe.widen.isRef(defn.QuotedTypeClass)

  def checkRedundancy(_match: Match): Unit = {
    val Match(sel, cases) = _match
    val selTyp = sel.tpe.widen.dealias

    if (!redundancyCheckable(sel)) return

    val targetSpace =
      if (ctx.explicitNulls || selTyp.classSymbol.isPrimitiveValueClass)
        project(selTyp)
      else
        project(OrType(selTyp, constantNullType, soft = false))

    // in redundancy check, take guard as false in order to soundly approximate
    def projectPrevCases(cases: List[CaseDef]): Space =
      cases.map { x =>
        if (x.guard.isEmpty) project(x.pat)
        else Empty
      }.reduce((a, b) => Or(List(a, b)))

    (1 until cases.length).foreach { i =>
      val prevs = projectPrevCases(cases.take(i))

      val pat = cases(i).pat

      if (pat != EmptyTree) { // rethrow case of catch uses EmptyTree
        val curr = project(pat)

        debug.println(s"---------------reachable? ${show(curr)}")
        debug.println(s"prev: ${show(prevs)}")

        var covered = simplify(intersect(curr, targetSpace))
        debug.println(s"covered: $covered")

        // `covered == Empty` may happen for primitive types with auto-conversion
        // see tests/patmat/reader.scala  tests/patmat/byte.scala
        if (covered == Empty && !isNullLit(pat)) covered = curr

        if (isSubspace(covered, prevs)) {
          report.warning(MatchCaseUnreachable(), pat.srcPos)
        }

        // if last case is `_` and only matches `null`, produce a warning
        // If explicit nulls are enabled, this check isn't needed because most of the cases
        // that would trigger it would also trigger unreachability warnings.
        if (!ctx.explicitNulls && i == cases.length - 1 && !isNullLit(pat) ) {
          simplify(minus(covered, prevs)) match {
            case Typ(`constantNullType`, _) =>
              report.warning(MatchCaseOnlyNullWarning(), pat.srcPos)
            case _ =>
          }
        }
      }
    }
  }
}
