package dotty.tools
package dotc
package transform
package eff

import core._
import Decorators._
import Symbols._
import Types._
import Names.Name

import ast.tpd
import tpd._
import core.Contexts
import core.Contexts.Context
import core.NameOps.{NameDecorator, TermNameDecorator}
import config.Printers.effect
import config.Printers.debug

import printing.{Showable, Printer}
import printing.Texts.Text

import transform.MegaPhase.MiniPhase

import StdNames.nme

import reporting.trace

import util.SimpleIdentitySet
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuilder, ListBuffer}

import java.{lang => jl}

/** A trick to make the implicitness of ctx invisible in `EscapeAnalysisEngine`. */
class EscapeAnalysisEngineBase(implicit ctx: Context) {
  val boxSymSet: SimpleIdentitySet[Symbol] = {
    SimpleIdentitySet.empty
     + ctx.requiredModule("scala.Long").requiredMethod("unbox")
     + ctx.requiredModule("scala.Int").requiredMethod("unbox")
     + ctx.requiredModule("scala.Int").requiredMethod("box")
  }

  val `scala.Int.int2long` = defn.IntClass.companionModule.requiredMethod("int2long")

  val ObjectRef_create = ctx.requiredModule("scala.runtime.ObjectRef").requiredMethod("create")
  val ObjectRef_elem = ctx.requiredClass("scala.runtime.ObjectRef").requiredValue("elem")

  /** Special symbol used to represent `this` in the store */
  val thisStoreKey = ctx.newSymbol(NoSymbol, "<!this!>".toTermName, Flags.EmptyFlags, NoType)
  /** Special symbol used to represent local variables in the heap */
  val scopeHeapKey = ctx.newSymbol(NoSymbol, "<!scope!>".toTermName, Flags.EmptyFlags, NoType)
}

// TODO this engine should probably only be initialised /once/
class EscapeAnalysisEngine(_ctx: Context) extends EscapeAnalysisEngineBase()(_ctx) {
  import EscapeAnalysisEngine.{given _, _}

  def showResult(it: Any)(implicit ctx: Context): String =
    it match
      case r: (AV @unchecked) => s"{{{\n${AV.display(r)}\n}}}"
      case _ => s"{{{\n${ it }\n}}}"

  def showHeap(it: Any)(implicit ctx: Context): String = {
    it match {
      case MutRes(h, mrv, rs) =>
        val res = new StringBuilder
        inline def raw(str: String) = res ++= str
        inline def ln(str: String) = res ++= "\n" ++= str
        inline def blk(header: String)(thunk: => Unit) = {
          ln(header)
          if header.nonEmpty then raw(" ")
          raw("{{{")
          thunk
          ln("}}}")
        }

        raw("{{{")
        blk("#heap") { ln(Heap.display(h)) }
        mrv match {
          case MRValue.Skipped =>
            ln("#value skipped")
          case MRValue.Abort =>
            ln("#value abort")
          case MRValue.Proper(av) =>
            blk("#value proper") { ln(AV.display(av)) }
        }
        blk("#returns") {
          rs.foreach { case (k, v) =>
            val line = s"$k → "
            ln(line)
            raw(AV.display(v, line.length))
          }
        }
        ln("}}}")

        res.toString
      case _ => s"{{{\n${ it }\n}}}"
    }
  }

  def accumulate(
    tree: Tree,
    store: Store,
    heap: Heap,
    terminal: Boolean
  )(using ctx: Context, stack: Stack, cache: Cache, entrypoint: Entrypoint): MutRes = {
    def loop(
      tree: Tree,
      _heap: Heap = heap,
      _store: Store = store,
      _terminal: Boolean = false
    )(using ctx: Context, stack: Stack) =
      accumulate(tree, _store, _heap, _terminal)

    def loopTerminal(
      tree: Tree,
      _heap: Heap = heap,
      _store: Store = store,
    )(using ctx: Context, stack: Stack) =
      accumulate(tree, _store, _heap, terminal = true)

    type Ret = Map[Name, AV]
    def mergeReturns(ret1: Ret, ret2: Ret): Ret =
      if (ret2.size > ret1.size) mergeReturns(ret2, ret1)
      else {
        var res = ret1
        ret2.foreach { case (k, av1) =>
          res = res.updatedWith(k) {
            case None => Some(av1)
            case Some(av2) => Some(av1 merge av2)
          }
        }
        res
      }

    def mergeSeq(res1: MutRes, res2: MutRes): MutRes = {
      val mergedValues = (res1.value, res2.value) match {
        case (MRValue.Abort, _) | (_, MRValue.Abort) => MRValue.Abort
        case (_, MRValue.Proper(av)) => MRValue.Proper(av)
        case (MRValue.Proper(av), MRValue.Skipped) => MRValue.Proper(av)
        case (MRValue.Skipped, MRValue.Skipped) => MRValue.Skipped
      }

      MutRes(
        res1.heap merge res2.heap,
        mergedValues,
        mergeReturns(res1.returns, res2.returns)
      )
    }

    def maybeMergeSeq(res1: MutRes, nextRes: Heap => MutRes): MutRes =
      res1.value match {
        case MRValue.Abort => res1
        case _ =>
          val res2 = nextRes(res1.heap)
          mergeSeq(res1, res2)
      }


    /** Merge mut-results from two _branching_ codepaths. */
    def mergeAlt(res1: MutRes, res2: MutRes): MutRes = {
      val mergedValues = (res1.value, res2.value) match {
        case (MRValue.Proper(av1), MRValue.Proper(av2)) => MRValue.Proper(av1 merge av2)
        case (_, MRValue.Proper(av)) => MRValue.Proper(av)
        case (MRValue.Proper(av), _) => MRValue.Proper(av)
        case (MRValue.Abort, _) => MRValue.Abort
        case (_, MRValue.Abort) => MRValue.Abort
        case (MRValue.Skipped, MRValue.Skipped) => MRValue.Skipped
      }

      MutRes(
        res1.heap merge res2.heap,
        mergedValues,
        mergeReturns(res1.returns, res2.returns)
      )
    }

    inline def loopCached0(
      newHeap: Heap,
      ap: AP,
      sym: Symbol,
      abstractArgs: List[AV]
    )(
      onMiss: () => MutRes,
      onError: () => MutRes
    ): MutRes =
      (cache().get((newHeap, ap, sym, abstractArgs)): @unchecked) match {
        // Already computed....
        case Some(mr: MutRes) => mr
        // In a fixpoint, but no initial value ... most implementations of onError return bottom...
        case Some("err") => onError()
        // Not present ... possibly need to evaluate fixpoint.
        case None =>
          cache().update((heap, ap, sym, abstractArgs), "err")
          var check : MutRes = onMiss()
          var go = true
          // Do a fixpoint calculation here...
          while (go) {
            cache().update((heap, ap, sym, abstractArgs), check)
            val newEntry = onMiss()
            if (newEntry == check) {
              go = false
            } else {
              check = newEntry
            }
          }

          check
      }

    inline def loopCached(
      ap: AP,
      sym: Symbol,
      abstractArgs: List[AV]
    )(
      body: Tree,
      newStore: Store
    )(
      onError: () => MutRes
    )(using ctx: Context, stack: Stack): MutRes =
      loopCached0(heap, ap, sym, abstractArgs)(
        () => accumulate(body, newStore, heap, terminal = true) ,
        onError
      )

    def _analyze(tree: Tree, _heap: Heap = heap)(using ctx: Context) =
      analyse(tree, store, _heap)

    def analyseArgsIntoNewStore(
      args: List[Tree],
      params: List[Tree],
      sig: Sig,
      _store: Store = store
    ): (Store, List[AV], Option[Taint]) = {
      var res: Store = _store

      val abstractArgsBld = ListBuffer.empty[AV]
      val taint = new Taint

      val paramSigs = sig match {
        case Sig.Proper(l) => l
        case Sig.Empty | null => LazyList.continually(ParamSig(false, Sig.Empty))
      }

      var didTaint = false
      for (vp, arg, ps) <- params.lazyZip(args).lazyZip(paramSigs) do {
        // ! SSA-assumptions
        // NOTE b/c of SSA-assumption, expressions here necessarily evaluate to a value
        val abstractArg1 = loopTerminal(arg).value.expected_!
        val abstractArg0 =
          abstractArg1.map {
            case (ap, sp) if sp.isDirect =>
              val newTaints =
                if !ps.tainted then sp.taints
                else {
                  didTaint = true
                  sp.taints + taint
                }
              val newSig =
                ps.sig match {
                  case Sig.Empty => sp.sig
                  case _ => ps.sig merge sp.sig
                }

              ap -> sp.copy(taints = newTaints, sig = newSig)
            case o => o
          }

        abstractArgsBld += abstractArg0
        res = res.updated(vp.symbol, abstractArg0)
      }

      (res, abstractArgsBld.result, Option(taint).filter(_ => didTaint))
    }

    inline def mrvalue(value: => AV) =
      if (terminal) MRValue.Proper(value) else MRValue.Skipped

    val uncluttered = unclutter(tree, dropBlocks = false)
    trace(i"accumulate(${tersely(uncluttered)}; terminal=$terminal; ${store.keys.map(_.name).toList}%, %)", effect, showHeap) {

      effect.println("{{{")
      effect.println(i"#tree = {{{\n${uncluttered}\n}}}")
      effect.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      effect.println("}}}")

      def emptyRes(_heap: Heap = heap): MutRes =
        MutRes(heap, mrvalue(AV.Empty), Map.empty)

      uncluttered match {
        /// constructor
        case tree @ Apply(Select(_, ident), _) if ident == nme.CONSTRUCTOR =>
          // ! SSA-assumption
          MutRes(
            heap,
            mrvalue(_analyze(tree)),
            Map.empty
          )

        case tree: ValDef if tree.symbol.is(Flags.Mutable) =>
          val rhsMR = loopTerminal(tree.rhs)
          val rhsAV = rhsMR.value.expected_!
          MutRes(
            rhsMR.heap.merge(AP.Sym(scopeHeapKey), tree.symbol, rhsAV),
            mrvalue(AV(AP.Constant, LabelSet.empty)),
            Map.empty
          )

        case Assign(ident: Ident, rhs) =>
          // local variable assignment
          val rhsMR = loopTerminal(rhs)
          val rhsAV = rhsMR.value.expected_!
          MutRes(
            rhsMR.heap.merge(AP.Sym(scopeHeapKey), ident.symbol, rhsAV),
            mrvalue(AV(AP.Constant, LabelSet.empty)),
            Map.empty
          )

        case Assign(lhs, rhs) =>
          effect.println(i"#acc-assign {{{")
          effect.println(i"#lhs {${lhs.productPrefix}} {{{\n${lhs}\n}}}")
          effect.println(i"#rhs {${rhs.productPrefix}} {{{\n${rhs}\n}}}")
          effect.println("}}}")
          MutRes(heap, mrvalue(AV(AP.Constant, LabelSet.empty)), Map.empty)

        case tree @ Apply(sel @ Select(obj, _), args) if sel.symbol.isSetter && sel.symbol.is(Flags.Mutable) =>
          // TODO array mutations have their own magick symbol-less methods
          val fieldName = sel.symbol.name.asTermName.getterName
          val d = sel.symbol.owner.info.decl(fieldName)
          val arg :: Nil = args // expecting only one arg for a setter

          val objMR = loop(obj, _terminal = true)
          val argMR = maybeMergeSeq(objMR, newHeap => loopTerminal(arg, _heap = newHeap))
          argMR.value match {
            case MRValue.Abort => objMR
            case MRValue.Skipped => sys.error("did not expect an MRValue.Skipped!")
            case MRValue.Proper(argAV) =>
              val objAV = objMR.value.expected_!

              val localsEscape =
                argAV.iterator.exists { case (_, sp) => !sp.taints.isEmpty }

              if localsEscape then {
                ctx.error("Tainted values escaping through assignment", tree.sourcePos)
              }

              val heap1 = argMR.heap
              val heap0 =
                (for {
                  (ap, sp) <- objAV.iterator
                  if sp.isDirect
                } yield ap).foldLeft(heap1) { (heap, ap) =>
                  heap.merge(ap, d.symbol, argAV)
                }

              MutRes(heap0, mrvalue(AV(AP.Constant, LabelSet.empty)), Map.empty)

          }

        case tree @ Apply(selT @ Select(objT, ident), args)
          if !objT.symbol.is(Flags.Module) =>
          def accumulateMethodCall(ap: AP, sig: Sig, objAV: AV): MutRes = trace("accumulateMethodCall") {
            effect.println(i"#details {{{")
            effect.println(i"#ap ${ap.display}")
            effect.println(i"#objAV {{{\n${objAV.display}\n}}}")
            effect.println("}}}")

            val newStack = Stack(
              s"${selT.symbol.show}:${selT.sourcePos.line}"
                :: stack.elements
            )

            (ap: @unchecked) match {
              case AP.Tree(clos @ Closure(env, closRef, _)) =>
                // TODO: test the case when the method cannot be statically determined
                val closDef = closRef.symbol.defTree.asInstanceOf[DefDef]
                val vparams :: Nil = closDef.vparamss

                effect.println(i"#clos-data {{{")
                effect.println(i"#sig ${sig}")
                effect.println(i"#vparams [${vparams.length}] ${vparams}%, %")
                effect.println(i"#env [${env.length}] $env%, %")
                effect.println("}}}")

                val (newStore1: Store, abstractArgs: List[AV], taintOpt) =
                  analyseArgsIntoNewStore(args, vparams.drop(env.length), sig)

                val newStore0 = {
                  val envSz = env.length
                  val envSyms = vparams.take(envSz).map(_.symbol)
                  val envAVs = new Array[AV | Null](envSz)

                  objAV.iterator.foreach { kv =>
                    val (ap, sp) = kv
                    envSyms.zipWithIndex.foreach { case (sym, idx) =>
                      if sp.labels.weak.contains(sym.name) then
                        envAVs(idx) = envAVs(idx) match {
                          case av: AV => av.merge(ap, sp)
                          case null => AV(kv)
                        }
                    }
                  }

                  var newStore = newStore1
                  envSyms.zipWithIndex.foreach { case (sym, idx) =>
                    newStore = envAVs(idx) match {
                      case av: AV => newStore.updated(sym, av)
                      case null => newStore - sym
                    }
                  }

                  newStore
                }

                // TODO: justify why we don't prune the AV here
                loopCached0(heap, ap, closRef.symbol, abstractArgs)(
                  onMiss = { () =>
                    val res =
                      loopTerminal(closDef.rhs, _store = newStore0)(using ctx, newStack)
                    (taintOpt, res.value) match {
                      case (None, _) => res
                      case (_, MRValue.Abort) => res
                      case (Some(t), MRValue.Proper(av)) =>
                        val untaintedAV = av.map {
                          case (ap, sp) if sp.taints.contains(t) =>
                            val newStack = Stack(
                              i"$tree :${tree.sourcePos.line}"
                              :: stack.elements
                            )
                            ctx.error(LocalValueEscapesMsg(t, newStack), entrypoint.tree.sourcePos)
                            ap -> sp.copy(taints = sp.taints - t)
                          case o => o
                        }
                        res.copy(value = MRValue.Proper(untaintedAV))
                      case other => sys.error(s"unexpected result: $other")
                    }
                  },
                  onError = { () =>
                    effect.println(i"#! StackOverflow on recursive closure: {{{")
                    effect.println(i"#clos {{{\n${clos}\n}}}")
                    effect.println("}}}")
                    MutRes(heap, MRValue.Proper(AV.Empty), Map.empty)
                  }
                )

              case AP.Tree(newT @ New(clsT)) =>
                val methSym = selT.symbol.denot.matchingMember(newT.tpe)
                val methDef = methSym.defTree.asInstanceOf[DefDef]
                val cstrDef = clsT.symbol.primaryConstructor.defTree.asInstanceOf[DefDef]

                val (newStore1: Store, abstractArgs: List[AV], _) =
                  analyseArgsIntoNewStore(args, methDef.vparamss.head, sig)

                // we filter out other "direct" values, as they cannot possibly be the value of `this`
                val newStore0 =
                  newStore1.updated(thisStoreKey, objAV)

                loopCached(ap, methSym, abstractArgs)(methDef.rhs, newStore0)(
                  onError = { () =>
                    effect.println(i"#! StackOverflow on recursive method: {{{")
                    effect.println(i"#methDef.rhs {{{\n${methDef.rhs}\n}}}")
                    effect.println("}}}")
                    MutRes(heap, MRValue.Proper(AV.Empty), Map.empty)
                  }
                )(using ctx, newStack)

              case AP.Sym(sym) =>
                val info = ctx.atPhase(ctx.postTyperPhase) { sym.denot.info }
                if (ident == nme.apply && defn.isFunctionType(info)) {
                  // TODO assuming that closures/constructors have exactly one parameter list
                  val AppliedType(_, params) = info // assuming that a "function type" is always an applied type
                  // TODO this is outright wrong
                  var res = MutRes(heap, MRValue.Proper(AV.Empty), Map.empty)
                  for {
                    (param, arg) <- params lazyZip args // discards the result type parameter
                    if !param.hasAnnotation(defn.LocalAnnot)
                  } do {
                    // TODO SSA-assumption
                    res = mergeAlt(res, loopTerminal(arg)(using ctx, newStack))
                  }

                  res
                } else {
                  effect.println(i"#!sym-method-call")
                  // TODO retrieve signatures for arbitrary methods
                  val initial =
                    MutRes(heap, MRValue.Proper(AV(ap, LabelSet.empty)), Map.empty)

                  args.foldLeft(initial) {
                    (av, arg) => mergeAlt(av, loop(arg)(using ctx, newStack))
                  }
                }

              case AP.Constant =>
                val initial =
                  MutRes(heap, MRValue.Proper(AV(AP.Constant, LabelSet.empty)), Map.empty)

                args.foldLeft(initial) {
                  (av, arg) => mergeAlt(av, loopTerminal(arg)(using ctx, newStack))
                }
            }
          }

          effect.println(i"#!method-call")
          effect.println(i"#sel.symbol ${selT.symbol}")
          effect.println(i"#selT.symbol.owner ${selT.symbol.owner}")

          def apAgreesWithClass(ap: AP, sym: Symbol): Boolean = {
            ap match {
              case AP.Tree(New(tpt)) =>
                val ci = tpt.tpe.asInstanceOf[TypeRef].underlying.asInstanceOf[ClassInfo]
                ci.cls.derivesFrom(selT.symbol.owner)
              case AP.Tree(_: Closure) =>
                // TODO implement
                true
              case _ => false
            }
          }

          val selectorOwnerClassSym = {
            val res = selT.symbol.owner
            res.ensuring(_.isClass)
          }

          // TODO take effects into account
          val objAV = {
            effect.println(i"#:object")
            loop(objT, _terminal = true).value.expected_!
          }
          var res = MutRes(Heap.Empty, MRValue.Proper(AV.Empty), Map.empty)
          for {
            (ap, sp) <- objAV.iterator
            if sp.isDirect
            && apAgreesWithClass(ap, selectorOwnerClassSym)
          } do {
            res = mergeAlt(res, accumulateMethodCall(ap, sp.sig, objAV))
          }

          res

        case tree @ Apply(fun, args) if !fun.symbol.defTree.isEmpty =>

          val fsym = fun.symbol
          val funDef = fsym.defTree.asInstanceOf[DefDef] // StoreAnnotations phase only adds this annot to DefDefs

          effect.println(i"#:acc:function-call {{{")
          effect.println(i"#funDef {${funDef.productPrefix}} {{{\n${funDef}\n}}}")
          effect.println("}}}")

          val sig =
            if !fsym.hasAnnotation(ctx.definitions.LocalParamsAnnot) then Sig.Empty
            else {
              val vparams :: Nil = funDef.vparamss
              val paramSigs =
                vparams.map { p =>
                  p.symbol.getAnnotation(ctx.definitions.LocalParamsAnnot) match {
                    case Some(annot) =>
                      val Apply(_, Literal(Constants.Constant(c)) :: Nil) = annot.tree
                      val arg = c.asInstanceOf[String] // unwrap the constant
                      val lpIndices = arg.split(",").iterator.map(_.toInt).toSet
                      val arity =
                        // TODO doesn't work for XXL, need to use time travel
                        ctx.definitions.scalaClassName(p.symbol.info).functionArity

                      val paramSigs =
                        (0 to (arity - 1)).iterator
                          .map { i =>
                            ParamSig(lpIndices contains i, Sig.Empty)
                          }
                          .toList

                      ParamSig(false, Sig.Proper(paramSigs))
                    case None =>
                      ParamSig(false, Sig.Empty)
                  }
                }

              Sig.Proper(paramSigs)
            }

          val (newStore: Store, abstractArgs: List[AV], _) =
            analyseArgsIntoNewStore(args, funDef.vparamss.head, sig)

          val newStack = Stack(
            s"${funDef.symbol.show}:${funDef.sourcePos.line}"
              :: stack.elements
          )

          loopCached0(heap, AP(fun.symbol), fun.symbol, abstractArgs)(
            onMiss = { () =>
              loopTerminal(
                tree = funDef.rhs,
                _store = newStore
              )(using ctx, newStack)
            },
            onError = { () =>
              effect.println(i"#! Run out of abstract stack when interpreting ${fun.symbol}")
              emptyRes() // NOTE! recursion-termination
            }
          )

        case If(cond, thenp, elsep) =>
          val res1 = loop(cond)
          val done = res1.value == MRValue.Abort

          // TODO [#C] test the code for exiting from if-conditions
          if (done) res1 else {
            val heap1 = res1.heap
            mergeAlt(
              loopTerminal(thenp, heap),
              loopTerminal(elsep, heap)
            )
          }

        case Return(expr, from) =>
          val exprMR = loopTerminal(expr)
          exprMR.value.unskipped match {
            case MRValue.Abort => exprMR
            case MRValue.Proper(exprAV) =>
              MutRes(
                exprMR.heap,
                MRValue.Abort,
                exprMR.returns.updated(
                  from.symbol.name,
                  exprAV
                )
              )
          }

        case Labeled(bind, tree) =>
          val res1 = loop(tree, _terminal = terminal)

          val blockLabel = bind.symbol.name
          val returnedAV =
            res1.returns.getOrElse(blockLabel, {
              // TODO ASK: is it ever possible to have a Labeled that has no associated returns?
              effect.println("#!!!labeled-no-returns")
              AV.Empty
            })

          def result(av: AV) =
            res1.copy(
              value = MRValue.Proper(av),
              returns = res1.returns - blockLabel
            )


          res1.value.unskipped match {
            case MRValue.Abort =>
              result(returnedAV)
            case MRValue.Proper(av) =>
              result(av merge returnedAV)
          }

        case Block(stmts, expr) =>
          var res: MutRes = null
          var curStore = store
          var curHeap = heap
          var returns: NSRs = Map.empty

          val iter = stmts.iterator
          while res == null && iter.hasNext do {
            iter.next match {
              case tree: ValDef if !tree.symbol.is(Flags.Mutable) =>
                val res1 = loopTerminal(tree.rhs, _store = curStore, _heap = curHeap)
                res1.value.unskipped match {
                  case MRValue.Abort =>
                    res = res1
                  case MRValue.Proper(av) =>
                    curStore = curStore.updated(tree.symbol, av)
                    curHeap = res1.heap
                    returns = mergeReturns(returns, res1.returns)
                }
              case tree =>
                // TODO clean this spaghetti up
                val res1 = loop(tree, _store = curStore, _heap = curHeap)
                res1.value match {
                  case MRValue.Abort =>
                    res = res1
                  case _ =>
                    curHeap = res1.heap
                    returns = mergeReturns(returns, res1.returns)
                }
            }
          }

          if res != null then res else {
            val res1 = loopTerminal(expr, _heap = curHeap, _store = curStore)
            res1.copy(returns = mergeReturns(returns, res1.returns))
          }

        case Apply(thrw, _) if thrw.symbol == defn.throwMethod =>
          // TODO handle throws
          // note: we need to have this stub here b/c pat-mat emits throws
          MutRes(heap, MRValue.Abort, Map.empty)

        case tree =>
          debug.println(i"#:acc:defaulting-to-ana")
          MutRes(heap, mrvalue(_analyze(tree)), Map.empty)
      }
    }
  }

  def analyse(
    tree: Tree,
    store: Store,
    heap: Heap
  )(using ctx: Context, stack: Stack, cache: Cache, entrypoint: Entrypoint): AV = {
    def loop(
      tree: Tree,
      _store: Store = store,
      _heap: Heap = heap
    )(using ctx: Context, stack: Stack) =
      accumulate(tree, _store, _heap, terminal = true).value.expected_!

    val uncluttered = unclutter(tree, dropBlocks = true)

    def heapLookup(sym: Symbol)(implicit ctx: Context): AV =
      heap.get(AP.Sym(scopeHeapKey), sym) match {
        case None =>
          effect.println(i"#!!! $sym#${sym.##} absent from local mutable heap!")
          ctx.error(s"$sym#${sym.##} absent from local mutable heap!")
          AV.Empty
        case Some(av) => av
      }

    def storeLookup(storeKey: Symbol)(implicit ctx: Context): AV =
      store.get(storeKey) match {
        case None =>
          effect.println(i"#!!! $storeKey#${storeKey.##} absent from the store!")
          ctx.error(s"$storeKey#${storeKey.##} absent from the store!")
          AV.Empty
        case Some(av) => av
      }

    def storeMemberLookup(storeKey: Symbol, fieldSym: Symbol, fieldName: Name)(implicit ctx: Context): AV = {
      store.get(storeKey) match {
        case None =>
          effect.println(i"#!!! $storeKey#${storeKey.##} absent from the store!")
          ctx.error(s"$storeKey#${storeKey.##} absent from the store!")
          AV.Empty
        case Some(av) =>
          val res =
            if (!tree.symbol.is(Flags.Mutable)) {
              av.filter { case (_, sp) => sp.labels.weak.contains(fieldName) }
            } else {
              val underlyingSym = {
                val name = tree.symbol.name.asTermName.getterName
                tree.symbol.owner.info.decl(name).symbol
              }
              av.iterator.foldLeft(AV.Empty) { (av, kv) => kv match {
                case (ap, sp) if sp.isDirect =>
                  av.merge(heap.getOrElse(ap, underlyingSym, {
                    effect.println(i"#!!! Heap misses a field: ${ap.display}.$underlyingSym#${underlyingSym.##}")
                    // TODO: error?
                    AV.Empty
                  }))
                case _ => av
              }}
            }
          if (res.isEmpty)
            // TODO error here?
            effect.println(i"#!!! $storeKey#${storeKey.##}.$fieldName has no value!")
          res
      }
    }

    trace(i"analyse(${tersely(uncluttered)}; ${store.keys.map(_.name).toList}%, %)", effect, showResult) {
      effect.println("{{{")
      effect.println(i"#tree = {{{\n${uncluttered}\n}}}")
      effect.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      effect.println(s"#heap = {{{\n${Heap.display(heap)}\n}}}")
      effect.println("}}}")

      uncluttered match {
        case tpd.Literal(_) => AV(AP.Constant, LabelSet.empty)

        case tree @ Apply(sel @ Select(obj, _), Nil) if sel.symbol.isGetter && sel.symbol.is(Flags.Mutable) =>
          val sym = sel.symbol.underlyingSymbol
          effect.println(i"#sel.symbol.## ${sel.symbol.##}")
          effect.println(i"#sel.symbol.name.## ${sel.symbol.name.##}")
          effect.println(i"#!sym.flags.flagsString ${sym.flags.flagsString}")

          var res = AV.Empty
          for {
            (ap, sp) <- loop(obj).iterator
            if sp.isDirect
          } do {
            res = res.merge(heap.getOrElse(ap, sym, {
              effect.println(i"#!!! Mutable symbol absent from heap! AP: ${ap.display} ; sym: ${sym}")
              AV.Empty
            }))
          }

          res

        case This(_) => storeLookup(thisStoreKey)

        case tree @ Ident(_) if tree.symbol.is(Flags.Mutable) =>
          heapLookup(tree.symbol)

        case tree @ Ident(_) if tree.symbol.is(Flags.Module) =>
          val rhsT = tree.symbol.defTree.asInstanceOf[ValDef].rhs
          val Apply(Select(newT, _), _) = rhsT
          val ap = AP.Tree(newT)
          AV(ap, LabelSet.empty)

        case tree @ Select(This(_), name) =>
          // assuming that we have a member
          val sym = tree.symbol
          effect.println(i"#:member-symbol-select {${sym}#${sym.##}}")
          storeMemberLookup(thisStoreKey, tree.symbol, name)

        case tree @ Ident(name) if tree.symbol.owner.isClass =>
          // assuming that we have a member
          val sym = tree.symbol
          effect.println(i"#:member-symbol-ident {${sym}#${sym.##}}")
          storeMemberLookup(thisStoreKey, tree.symbol, name)

        case tree @ Ident(_) =>
          effect.println(i"#ident-data {{{")
          effect.println(i"#tree.name {{{\n${tree.name}\n}}}")
          effect.println(i"#tree.qualifier {{{\n${tree.qualifier}\n}}}")
          effect.println("}}}")

          store.getOrElse(tree.symbol,
            tree.symbol.defTree match {
              case EmptyTree => sys.error(i"missing def tree for {${tree.symbol}} shouldn't happen, missing -Yretain-trees")
              case dt: ValDef =>
                effect.println(i"going into def tree of $tree")
                accumulate(
                  dt.rhs,
                  store = store,
                  heap = heap,
                  terminal = true
                ).value.expected_!
            }
          )

        case tree @ Apply(createT, args) if createT.symbol == ObjectRef_create =>
          val List(arg) = args
          val argAV = loop(arg)
          val ap = AP.Tree(tree)
          AV(ap, LabelSet.empty) merge argAV.iterator.map {
            case (argAP, sp) => argAP -> (sp offset ObjectRef_elem.name)
          }

        /// constructor
        case tree @ Apply(fun @ Select(new_, ident), args) if ident == nme.CONSTRUCTOR =>
          val fsym = fun.symbol
          assert(fsym.isPrimaryConstructor)
          val constructorDef = fsym.defTree.asInstanceOf[DefDef]
          val constructorVparams :: Nil = constructorDef.vparamss

          val constructorArgPairs =
            constructorVparams.lazyZip(args).map {
              case (p, arg) => p -> loop(arg)
            }.toList

          def constructorAssignmentsIter: Iterator[(AP, Spec)] = {
            val newStore = {
              var res = store
              for {
                (p, av) <- constructorArgPairs
              } do {
                res = res.updated(p.symbol, av)
              }
              res
            }

            effect.println("#constructor-iteration {{{")
            effect.println(i"#constructorDef.rhs (${constructorDef.rhs.productPrefix}) {{{\n${constructorDef.rhs}\n}}}")
            val collected = ArrayBuilder.make[Iterator[(AP, Spec)]]
            def iterate(tree: Tree): Unit =
              tree match {
                case Block(stmts, expr) =>
                  stmts.foreach(iterate)
                  iterate(expr)
                case Assign(Select(This(_), name), rhs) =>
                  collected += loop(rhs, newStore).iterator.map {
                    case (ap, sp) => ap -> (sp offset name)
                  }
                case Assign(lhs, rhs) =>
                  effect.println(i"#assign:")
                  effect.println(i"##lhs (${lhs.productPrefix}) {{{\n${lhs}\n}}}")
                  effect.println(i"##rhs (${rhs.productPrefix}) {{{\n${rhs}\n}}}")
                case tree =>
                  effect.println(i"#seeing: {${tree.productPrefix}} {{{\n${tree}\n}}}")
              }
            iterate(constructorDef.rhs)
            effect.println("}}}")

            collected.result.iterator.flatten
          }

          assert(args.hasSameLengthAs(constructorVparams))
          AV.merge(
            AV(AP(new_), LabelSet.empty).iterator
              ++ constructorAssignmentsIter
              ++ {
                for {
                  (param, av) <- constructorArgPairs
                  (ap, sp) <- av.iterator
                } yield
                  ap -> (sp offset param.name)
              }
          )

        case tree @ Closure(env, meth, _) =>
          val closureDefTree = meth.symbol.defTree.asInstanceOf[DefDef]
          effect.println(i"#closure {{{")
          effect.println(i"#tree ${tree}")
          effect.println(i"#closureDefTree {${closureDefTree.productPrefix}} {{{\n${closureDefTree}\n}}}")
          effect.println(i"#env $env%, %")
          effect.println("}}}")

          var res = AV(AP(tree), LabelSet.empty)
          for {
            (t, vt) <- env lazyZip closureDefTree.vparamss.head
            (ap, ls) <- loop(t).iterator
          } do
            res = res.merge(ap, ls offset vt.symbol.name)

          res

        case tree @ Select(_, _) if tree.symbol == defn.Any_isInstanceOf =>
          AV(AP.Constant, LabelSet.empty)
      }
    }
  }

  def analyseDefinitionWithLocalParameters(tree: DefDef)(implicit ctx: Context): Tree = {
    val (localParams: LocalParams, localStore: Store) = {
      var res1: LocalParams = SimpleIdentitySet.empty
      var res2: Store = Map.empty
      for
        vparams <- tree.vparamss
        vp <- vparams
      do
        if vp.symbol.hasAnnotation(ctx.definitions.LocalAnnot) then res1 += vp.symbol
        res2 = res2.updated(vp.symbol, AV(AP(vp.symbol), LabelSet.empty))

      (res1, res2)
    }

    if (localParams.size != 0)
      effect.println(i"local params for ${tree.symbol} ==> ${localParams.toList}%, %")


    // import given Flags.FlagOps
    if (localParams.size > 0 || tree.symbol.hasAnnotation(ctx.definitions.EntryAnnot)) {
      effect.println(i"${tree.name}: detected local params")
      val cache = new Cache(mutable.HashMap[Cache.Key, Cache.Value]())
      val mutRes =
        accumulate(
          tree.rhs, localStore.toMap, Heap.Empty, terminal = true
        )(using
          ctx,
          Stack(s"${tree.symbol}:${tree.sourcePos.line}" :: Nil),
          cache,
          Entrypoint(tree)
        )
      val av = mutRes.value match {
        case MRValue.Proper(av) =>
          effect.println("#mut-res: proper")
          av
        case MRValue.Abort =>
          effect.println("#mut-res: abort")
          // TODO: the values here are escaping, need to check them
          AV.Empty
        case MRValue.Skipped => assert(false) // shouldn't happen when terminal
      }

      val resolved = AV.resolve(av, mutRes.heap)
      val escapees =
        resolved.self.keysIterator.filter(ap => localParams.contains(ap.maybeSym)).toSeq
      if (escapees.size > 0) {
        escapees.foreach { s => (s: @unchecked) match {
          case AP.Sym(sym) =>
            // ctx.error(i"(m) local escapes ($sym)", sym.sourcePos)
            ctx.reporter.flush()
        }}
      } else {
        effect.println("No locals escape")
      }
    }

    tree
  }

  private def finalExprOf(tree: Tree, skipLabeled: Boolean = false): Tree = {
    var finalExpr: Tree = tree
    def unblocked: Tree =
      finalExpr match {
        case Block(_, expr) => expr
        case Labeled(_, expr) if skipLabeled => expr
        case _ => null
      }

    var _block = unblocked
    while (_block != null) {
      finalExpr = _block
      _block = unblocked
    }
    finalExpr
  }

  def unclutter(tree: Tree, dropBlocks: Boolean)(implicit ctx: Context): Tree = {
    def loop(tree: Tree) = unclutter(tree, dropBlocks)
    tree match {
      case Typed(expr, _) => loop(expr)
      case Block(_, expr) if dropBlocks => loop(expr)
      case Apply(fun, expr :: Nil)
          if boxSymSet.contains(tree.symbol)
          || tree.symbol == `scala.Int.int2long`
          => expr
      case TypeApply(expr, _) => loop(expr)
      case Select(expr, ident) if ident == nme.asInstanceOf_ => expr
      case expr => expr
    }
  }
}

object EscapeAnalysisEngine {

  type Label = Names.Name

  case class LabelSet(
    weak: Set[Label],
    strong: Set[Label]
  ) {
    def +(label: Label) = LabelSet(weak = this.weak + label, strong = this.strong + label)

    def isEmpty: Boolean = weak.isEmpty && strong.isEmpty

    def display: String =
      if isEmpty then "{}" else s"{#${weak.size}/#${strong.size}}"
  }

  object LabelSet {
    val empty = new LabelSet(Set.empty, Set.empty)
    def merge(a: LabelSet, b: LabelSet): LabelSet =
      LabelSet(
        weak   = a.weak union b.weak,
        strong = a.strong intersect b.strong
      )
  }

  type LocalParams = SimpleIdentitySet[Symbol]

  enum AP extends Showable {
    case Sym(symbol: Symbol);
    case Tree(tree: tpd.Tree);
    case MutScope();
    case Constant;

    def maybeSym: Symbol =
      this match {
        case AP.Sym(s) => s
        case _ => NoSymbol
      }

    override def toText(printer: Printer): Text = {
      import printing._
      import Texts._

      val txt = this match {
        case Sym(v) => printer.toText(v)
        case Tree(v) => printer.toText(v)
        case MutScope() => Str(s"Scope#${this.##}")
        case Constant => Str("Constant")
      }

      Str("AP(") ~ txt ~ Str(")")
    }

    def display(implicit ctx: Context) =
      this match {
        case AP.Sym(s) => s.name
        case AP.Tree(t) => s"${tersely(t)}#${t.##}"
        case MutScope() => s"<Scope#${this.##}>"
        case AP.Constant => "<Constant>"
      }
  }

  object AP {
    def apply(symbol: Symbol) = AP.Sym(symbol)
    def apply(tree: tpd.Tree) = AP.Tree(tree)
  }

  class Taint
  case class ParamSig(tainted: Boolean, sig: Sig)
  enum Sig {
    def merge(other: Sig): Sig =
      (this, other) match {
        case (Sig.Empty, Sig.Empty) => Sig.Empty
        case (s : Sig.Proper, Sig.Empty) => s
        case (Sig.Empty, s : Sig.Proper) => s
        case _ => sys.error("merging signatures not thought of yet")
      }


    case Empty;
    case Proper(sigs: List[ParamSig]);
  }

  type Taints = SimpleIdentitySet[Taint]
  case class Spec(labels: LabelSet, taints: Taints, sig: Sig) {
    def isDirect: Boolean = labels.strong.isEmpty

    def merge(other: Spec): Spec = {
      Spec(
        LabelSet.merge(this.labels, other.labels),
        this.taints ++ other.taints,
        this.sig merge other.sig
      )
    }

    def offset(l: Label) = copy(labels = labels + l)

    def display = labels.display
  }

  class AV(val self: Map[AP, Spec]) extends AnyVal {
    def iterator = self.iterator

    def map(f: ((AP, Spec)) => (AP, Spec)): AV =
      AV(this.self.map(f))

    def filter(f: ((AP, Spec)) => Boolean): AV =
      AV(this.self.filter(f))

    def merge(ap: AP, sp1: Spec): AV =
      AV(this.self.updatedWith(ap) {
        case None => Some(sp1)
        case Some(sp2) =>
          Some(sp1 merge sp2)
      })

    def merge(other: AV): AV =
      if (other.self.size < this.self.size)
        merge(other.iterator)
      else
        other.merge(this.iterator)

    def merge(other: Iterator[(AP, Spec)]): AV = {
      var res = this
      for (ap, sp) <- other do
        res = res.merge(ap, sp)
      res
    }

    def isEmpty: Boolean = self.isEmpty

    def display(using Context): String = AV.display(this, 0)
  }

  object AV {
    val Empty = AV(Map.empty)

    def apply(ap: AP, ls: LabelSet): AV =
      apply((ap, Spec(ls, SimpleIdentitySet.empty, Sig.Empty)))
    def apply(kv: (AP, Spec)): AV = AV(Map(kv))
    def apply(map: Map[AP, Spec]): AV = new AV(map)

    def resolve(av: AV, heap: Heap): AV = {
      merge(
        av.iterator
          ++ (for {
            (ap1, sp1) <- av.iterator
            (sym, av2) <- heap.self.get(ap1).iterator.flatten
          } yield av2.iterator.map { case (ap2, sp2) =>
              // TODO why isn't `sym` used here? why does that work?
              ap2 -> Spec(
                labels = LabelSet(
                  weak = sp1.labels.weak union sp2.labels.weak,
                  strong = sp1.labels.strong intersect sp2.labels.strong
                ),
                // we don't care about any of those when we resolve an AV
                taints = SimpleIdentitySet.empty,
                sig = Sig.Empty
              )
          }).flatten
      )
    }

    def merge(labels: Iterator[(AP, Spec)]): AV = {
      val resIter =
        for
          (a, g) <- labels.toArray.groupBy(_._1).iterator
        yield
          a -> (g.map(_._2).reduce(_ merge _))

      AV(resIter.toMap)
    }

    def display(av: AV, indent: Int = 0)(implicit ctx: Context): String = {
      val res = new StringBuffer
      val spaces = " " * indent
      var nl_? = false
      def nl() =
        if nl_? then {
          res append "\n"
          res append spaces
        }
        nl_? = true

      for (k, v) <- av.iterator do
        nl()
        res append k.display
        res append s" ← ${v.display}"

      res.toString
    }
  }

  class Cache(
    val self: mutable.Map[Cache.Key, Cache.Value]
  ) extends AnyVal {
    def apply() = self
  }

  object Cache {
    type Key = (Heap, AP, Symbol, List[AV])
    type Value = MutRes | "err"
  }

  enum MRValue {
    def expected_! = this match {
      case Proper(av) => av
      case _ => sys.error("expected MRValue to be proper!")
    }

    inline def improper: Abort.type | Skipped.type = this match {
      case mrv: Proper => sys.error("did not expect an MRValue.Proper!")
      case Skipped => Skipped
      case Abort => Abort
    }

    inline def unskipped: Abort.type | Proper = this match {
      case Skipped => sys.error("did not expect an MRValue.Skipped!")
      case Abort => Abort
      case mrv: Proper => mrv
    }

    inline def expectedOr(thunk: => AV) = this match {
      case Proper(av) => av
      case Abort => thunk
      case _ => sys.error("did not expect an MRValue.Skipped!")
    }

    case Skipped;
    case Abort;
    case Proper(value: AV);
  }
  /** Non-standard returns */
  type NSRs = Map[Name, AV]
  case class MutRes(heap: Heap, value: MRValue, returns: NSRs)

  class Heap (val self: Map[AP, Map[Symbol, AV]]) extends AnyVal {
    def get(ap: AP, sym: Symbol): Option[AV] =
      for {
        inner <- self.get(ap)
        res <- inner.get(sym)
      } yield res

    def getOrElse(ap: AP, sym: Symbol, elsep: => AV): AV =
      self.get(ap) match {
        case None => elsep
        case Some(inner) => inner.getOrElse(sym, elsep)
      }

    def merge(other: Heap): Heap = {
      var res: Map[AP, Map[Symbol, AV]] = other.self
      for { (ap, inner1) <- this.self } do {
        res = res.updatedWith(ap) {
          case None => Some(inner1)
          case Some(inner2) =>
            var innerRes = inner2
            for { (sym, av1) <- inner1 } do {
              innerRes = innerRes.updatedWith(sym) {
                case None => Some(av1)
                case Some(av2) => Some(av1 merge av2)
              }
            }
            Some(innerRes)
        }
      }
      Heap(res)
    }

    def merge(ap: AP, sym: Symbol, av: AV): Heap =
      updatedWith(ap, sym, av) { _ merge av }

    def updatedWith(ap: AP, sym: Symbol, default: AV)(thunk: AV => AV): Heap =
      Heap(self.updatedWith(ap) {
        case None => Some(Map(sym -> default))
        case Some(inner) => Some(inner.updatedWith(sym) {
          case None => Some(default)
          case Some(ap) => Some(thunk(ap))
        })
      })

    def iterator: Iterator[((AP, Symbol), AV)] =
      self.iterator.flatMap {
        case (ap, inner) => inner.iterator.map {
          case (sym, av) => (ap, sym) -> av
        }
      }
  }

  object Heap {
    val Empty = Heap(Map.empty)

    def display(heap: Heap)(implicit ctx: Context): String = {
      val buf = new StringBuffer

      var nl_? = false
      def nl() = {
        if nl_? then buf append "\n"
        nl_? = true
      }

      for ((ap, sym), v) <- heap.iterator
      do {
        nl()
        val ln = s"${ap.display}.${sym.name}#${sym.##} → "
        buf append ln
        buf append AV.display(v, ln.length)
      }

      buf.toString
    }
  }

  type Store = Map[Symbol, AV]
  object Store {
    def display(store: Store, indent: Int)(implicit ctx: Context): String =
      val buf = new StringBuffer
      val spaces = " " * indent

      var nl_? = false
      def nl() =
        if nl_? then buf append "\n"
      nl_? = true

      for (k, v) <- store
      do
        nl()
        val ln = s"${spaces}${k.name} → "
        buf append ln
        buf append AV.display(v, ln.length)
      buf.toString
  }

  object HasSym {
    def unapply(tree: Tree)(implicit ctx: Context): Some[Symbol] = Some(tree.symbol)
  }

  def tersely(tree: Tree)(implicit ctx: Context): String =
    def prefix = tree.productPrefix
    tree match {
      case _: Ident => i"$prefix( $tree )"
      case _ => s"$prefix(…)"
    }

  class Entrypoint(val tree: Tree) extends AnyVal

  case class Stack(elements: List[String])

  class LocalValueEscapesMsg(
    taint: Taint,
    stack: Stack
  ) extends reporting.Message(reporting.ErrorMessageID.NoExplanationID) {

    def msg: String = {
      def renderStack(): String = {
        val sb = new StringBuffer
        sb append "Traceback:"
        for el <- stack.elements do {
          sb append "\n"
          sb append "===> "
          sb append el
        }
          sb.toString
      }

      s"Tainted value escapes (taint: ${taint.##})\n${renderStack()}"
    }

    def explain: String = ""
    val kind: String = ""

    override def toString(): String = msg
  }
}
