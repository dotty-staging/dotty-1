package dotty.tools
package dotc
package cc

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types._
import Symbols._
import StdNames._
import Decorators._
import config.Printers.{capt, recheckr}
import ast.{tpd, untpd, Trees}
import NameKinds.{DocArtifactName, OuterSelectName, DefaultGetterName}
import Trees._
import scala.util.control.NonFatal
import typer.ErrorReporting._
import util.Spans.Span
import util.{SimpleIdentitySet, EqHashMap, SrcPos, Property}
import util.Chars.*
import transform.*
import transform.SymUtils.*
import scala.collection.mutable
import reporting._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import CaptureSet.CompareResult

object CheckCaptures:
  import ast.tpd.*

  case class Env(owner: Symbol, captured: CaptureSet, isBoxed: Boolean, outer: Env):
    def isOpen = !captured.isAlwaysEmpty && !isBoxed

  final class SubstParamsMap(from: BindingType, to: List[Type])(using Context)
  extends ApproximatingTypeMap:
    def apply(tp: Type): Type = tp match
      case tp: ParamRef =>
        if tp.binder == from then to(tp.paramNum) else tp
      case tp: NamedType =>
        if tp.prefix `eq` NoPrefix then tp
        else tp.derivedSelect(apply(tp.prefix))
      case _: ThisType =>
        tp
      case _ =>
        mapOver(tp)

  /** Check that a @retains annotation only mentions references that can be tracked
   *  This check is performed at Typer.
   */
  def checkWellformed(ann: Tree)(using Context): Unit =
    for elem <- retainedElems(ann) do
      elem.tpe match
        case ref: CaptureRef =>
          if !ref.canBeTracked then
            report.error(em"$elem cannot be tracked since it is not a parameter or a local variable", elem.srcPos)
        case tpe =>
          report.error(em"$tpe is not a legal type for a capture set", elem.srcPos)

  /** If `tp` is a capturing type, check that all references it mentions have non-empty
   *  capture sets.
   *  This check is performed after capture sets are computed in phase cc.
   */
  def checkWellformedPost(tp: Type, pos: SrcPos)(using Context): Unit = tp match
    case CapturingType(parent, refs) =>
      for ref <- refs.elems do
        if (ref.captureSet frozen_<:< CaptureSet.empty) == CompareResult.OK then
          report.error(em"$ref cannot be tracked since its capture set is empty", pos)
        else if parent.captureSet.accountsFor(ref) then
          report.warning(em"redundant capture: $parent already accounts for $ref", pos)
    case _ =>

  def checkWellformedPost(ann: Tree)(using Context): Unit =
    /** The lists `elems(i) :: prev.reerse :: elems(0),...,elems(i-1),elems(i+1),elems(n)`
     *  where `n == elems.length-1`, i <- 0..n`.
     */
    def choices(prev: List[Tree], elems: List[Tree]): List[List[Tree]] = elems match
      case Nil => Nil
      case elem :: elems =>
        List(elem :: (prev reverse_::: elems)) ++ choices(elem :: prev, elems)
    for case first :: others <- choices(Nil, retainedElems(ann)) do
      val firstRef = first.toCaptureRef
      val remaining = CaptureSet(others.map(_.toCaptureRef)*)
      if remaining.accountsFor(firstRef) then
        report.warning(em"redundant capture: $remaining already accounts for $firstRef", ann.srcPos)

  private inline val disallowGlobal = true

class CheckCaptures extends Recheck:
  thisPhase =>

  import ast.tpd.*
  import CheckCaptures.*

  def phaseName: String = "cc"
  override def isEnabled(using Context) = ctx.settings.Ycc.value

  def newRechecker()(using Context) = CaptureChecker(ctx)

  class CaptureChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    override def reinfer(tp: Type)(using Context): Type =

      def mapRefined(tp: RefinedType, core1: Type, rinfo1: Type): Type =
        if (rinfo1 ne tp.refinedInfo) && defn.isFunctionType(tp)
        then rinfo1.toFunctionType(isJava = false)
        else tp.derivedRefinedType(core1, tp.refinedName, rinfo1)

      val cleanType = new TypeMap:
        def apply(t: Type) = t match
          case AnnotatedType(parent, annot) if annot.symbol == defn.RetainsAnnot =>
            apply(parent)
          case _ =>
            mapOver(t)

      def addInnerVars(tp: Type): Type = tp match
        case tp @ AppliedType(tycon, args) =>
          tp.derivedAppliedType(tycon, args.map(addVars))
        case tp @ RefinedType(core, _, rinfo) =>
          mapRefined(tp, addInnerVars(core), addVars(rinfo))
        case tp: MethodOrPoly =>
          tp.derivedLambdaType(resType = addVars(tp.resType))
        case tp: ExprType =>
          tp.derivedExprType(resType = addVars(tp.resType))
        case _ =>
          tp

      def addVars(tp: Type): Type =
        val tp1 = addInnerVars(tp)
        if tp1.canHaveInferredCapture
        then CapturingType(tp1, CaptureSet.Var())
        else tp1

      addVars(cleanType(tp))
    end reinfer

    private var curEnv: Env = Env(NoSymbol, CaptureSet.empty, false, null)

    private val myCapturedVars: util.EqHashMap[Symbol, CaptureSet] = EqHashMap()
    def capturedVars(sym: Symbol)(using Context) =
      myCapturedVars.getOrElseUpdate(sym,
        if sym.ownersIterator.exists(_.isTerm) then CaptureSet.Var()
        else CaptureSet.empty)

    def markFree(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if sym.exists then
        val ref = sym.termRef
        def recur(env: Env): Unit =
          if env.isOpen && env.owner != sym.enclosure then
            capt.println(i"Mark $sym with cs ${ref.captureSet} free in ${env.owner}")
            checkElem(ref, env.captured, pos)
            recur(env.outer)
        if ref.isTracked then recur(curEnv)

    def includeCallCaptures(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if curEnv.isOpen then
        val ownEnclosure = ctx.owner.enclosingMethodOrClass
        var targetSet = capturedVars(sym)
        if !targetSet.isAlwaysEmpty && sym.enclosure == ownEnclosure then
          targetSet = targetSet.filter {
            case ref: TermRef => ref.symbol.enclosure != ownEnclosure
            case _ => true
          }
        checkSubset(targetSet, curEnv.captured, pos)

    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert((cs1 <:< cs2) == CompareResult.OK, i"$cs1 is not a subset of $cs2")

    def checkElem(elem: CaptureRef, cs: CaptureSet, pos: SrcPos)(using Context) =
      val res = elem.singletonCaptureSet <:< cs
      if res != CompareResult.OK then
        report.error(i"$elem cannot be referenced here; it is not included in allowed capture set ${res.blocking}", pos)

    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos)(using Context) =
      val res = cs1 <:< cs2
      if res != CompareResult.OK then
        report.error(i"references $cs1 are not all included in allowed capture set ${res.blocking}", pos)

    override def recheckClosure(tree: Closure, pt: Type)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      recheckr.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt).capturing(cs)
        .showing(i"rechecked $tree, $result", capt)

    override def recheckIdent(tree: Ident)(using Context): Type =
      markFree(tree.symbol, tree.srcPos)
      if tree.symbol.is(Method) then includeCallCaptures(tree.symbol, tree.srcPos)
      super.recheckIdent(tree)

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      super.recheckValDef(tree, sym)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(sym)
      if !localSet.isAlwaysEmpty then curEnv = Env(sym, localSet, false, curEnv)
      try super.recheckDefDef(tree, sym)
      finally curEnv = saved

    override def recheckClassDef(tree: TypeDef, impl: Template, sym: ClassSymbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(sym)
      if !localSet.isAlwaysEmpty then curEnv = Env(sym, localSet, false, curEnv)
      try super.recheckClassDef(tree, impl, sym)
      finally curEnv = saved

    override def instantiate(mt: MethodType, argTypes: => List[Type])(using Context): Type =
      if mt.isResultDependent then SubstParamsMap(mt, argTypes)(mt.resType)
      else mt.resType

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val sym = tree.symbol
      includeCallCaptures(sym, tree.srcPos)
      val cs = if sym.isConstructor then capturedVars(sym.owner) else CaptureSet.empty
      super.recheckApply(tree, pt).capturing(cs)

    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val saved = curEnv
      if pt.needsBox && !curEnv.isBoxed then // ^^^ refine?
        curEnv = Env(NoSymbol, CaptureSet.Var(), true, curEnv)
      try
        val res = super.recheck(tree, pt)
        if curEnv.isOpen then assertSub(res.boxedCaptured, curEnv.captured)
        res
      finally curEnv = saved

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      super.checkUnit(unit)
      PostRefinerCheck.traverse(unit.tpdTree)

    def checkNotGlobal(tree: Tree, allArgs: Tree*)(using Context): Unit =
      if disallowGlobal then
        tree match
          case LambdaTypeTree(_, restpt) =>
            checkNotGlobal(restpt, allArgs*)
          case _ =>
            for ref <- tree.tpe.captureSet.elems do
              val isGlobal = ref match
                case ref: TermRef =>
                  ref.isRootCapability || ref.prefix != NoPrefix && ref.symbol.hasAnnotation(defn.AbilityAnnot)
                case _ => false
              val what = if ref.isRootCapability then "universal" else "global"
              if isGlobal then
                val notAllowed = i" is not allowed to capture the $what capability $ref"
                def msg = tree match
                  case tree: InferredTypeTree =>
                    i"""inferred type argument ${tree.tpe}$notAllowed
                        |
                        |The inferred arguments are: [$allArgs%, %]"""
                  case _ => s"type argument$notAllowed"
                report.error(msg, tree.srcPos)

    object PostRefinerCheck extends TreeTraverser:
      def traverse(tree: Tree)(using Context) =
        tree match
          case _: InferredTypeTree =>
          case tree: TypeTree =>
            tree.tpe.foreachPart(
              checkWellformedPost(_, tree.srcPos))
            tree.tpe.foreachPart {
              case AnnotatedType(_, annot) =>
                checkWellformedPost(annot.tree)
              case _ =>
            }
          case tree1 @ TypeApply(fn, args) if disallowGlobal =>
            for arg <- args do
              //println(i"checking $arg in $tree: ${arg.tpe.captureSet}")
              checkNotGlobal(arg, args*)
          case _ =>
        traverseChildren(tree)

    def postRefinerCheck(tree: tpd.Tree)(using Context): Unit =
      PostRefinerCheck.traverse(tree)

  end CaptureChecker
end CheckCaptures
