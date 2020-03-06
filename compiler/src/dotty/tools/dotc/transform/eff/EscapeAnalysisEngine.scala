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
import core.NameOps.TermNameDecorator
import config.Printers.debug

import printing.{Showable, Printer}
import printing.Texts.Text

import transform.MegaPhase.MiniPhase

import StdNames.nme

import reporting.trace

import util.SimpleIdentitySet
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

  /** Special symbol used to represent `this` in the store */
  val thisStoreKey = ctx.newSymbol(NoSymbol, "<!this!>".toTermName, Flags.EmptyFlags, NoType)
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
    cache: Cache,
    heap: Heap,
    terminal: Boolean
  )(implicit ctx: Context): MutRes = {
    def loop(
      tree: Tree,
      _store: Store = store,
      _cache: Cache = cache,
      _heap: Heap = heap,
      _terminal: Boolean = false
    )(implicit ctx: Context) = accumulate(tree, _store, _cache, _heap, _terminal)

    // TODO keep only the symbols that don't have a defTree
    lazy val localSymbols = store.keys

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

    def mergeMRValues(mrv1: MRValue, mrv2: MRValue): MRValue =
      (mrv1, mrv2) match {
        case (MRValue.Abort, _) | (_, MRValue.Abort) => MRValue.Abort
        case (MRValue.Proper(av1), MRValue.Proper(av2)) => MRValue.Proper(av1 merge av2)
        case (MRValue.Proper(av), MRValue.Skipped) => MRValue.Proper(av)
        case (MRValue.Skipped, MRValue.Proper(av)) => MRValue.Proper(av)
        case (MRValue.Skipped, MRValue.Skipped) => MRValue.Skipped
      }

    def integrate(res1: MutRes, tree: Tree, _terminal: Boolean = false) =
      res1.value match {
        case MRValue.Abort => res1
        case _ =>
          val res2 = loop(tree, _heap = res1.heap, _terminal = _terminal)
          MutRes(
            res1.heap merge res2.heap,
            mergeMRValues(res1.value, res2.value),
            mergeReturns(res1.returns, res2.returns)
          )
      }

    /** Merge mut-results from two _branching_ codepaths. */
    def merge(res1: MutRes, res2: MutRes): MutRes = {
      val mergedValues = (res1.value, res2.value) match {
        case (MRValue.Abort, MRValue.Proper(av)) => MRValue.Proper(av)
        case (MRValue.Proper(av), MRValue.Abort) => MRValue.Proper(av)
        case _ => mergeMRValues(res1.value, res2.value)
      }

      MutRes(
        res1.heap merge res2.heap,
        mergedValues,
        mergeReturns(res1.returns, res2.returns)
      )
    }

    def _analyze(tree: Tree, _heap: Heap = heap)(implicit ctx: Context) = analyse(tree, store, cache, _heap)

    def analyseArgsIntoNewStore(
      args: List[Tree],
      params: List[Tree],
      _store: Store = store
    ): (Store, List[AV]) = {
      var res: Store = _store

      val abstractArgsBld = ListBuffer.empty[AV]
      for {
        (vp, arg) <- params lazyZip args // TODO assuming that closures/constructors have exactly one parameter list
      } do {
        // ! SSA-assumptions
        // NOTE b/c of SSA-assumption, expressions here necessarily evaluate to a value
        val abstractArg = loop(arg, _terminal = true).value.asInstanceOf[MRValue.Proper].value
        abstractArgsBld += abstractArg
        res = res.updated(vp.symbol, abstractArg)
      }

      (res, abstractArgsBld.result)
    }

    inline def mrvalue(value: => AV) =
      if (terminal) MRValue.Proper(value) else MRValue.Skipped

    val uncluttered = unclutter(tree, dropBlocks = false)
    trace(i"accumulate(${tersely(uncluttered)}; terminal=$terminal; ${store.keys.map(_.name).toList}%, %)", debug, showHeap) {

      debug.println("{{{")
      debug.println(i"#tree = {{{\n${uncluttered}\n}}}")
      debug.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      debug.println("}}}")


      uncluttered match {
        /// constructor
        case tree @ Apply(Select(_, ident), _) if ident == nme.CONSTRUCTOR =>
          // ! SSA-assumption
          MutRes(
            heap,
            mrvalue(_analyze(tree)),
            Map.empty
          )

        case Assign(lhs, rhs) =>
          debug.println(i"#!acc-assign {{{")
          debug.println(i"#lhs {${lhs.productPrefix}} {{{\n${lhs}\n}}}")
          debug.println(i"#rhs {${rhs.productPrefix}} {{{\n${rhs}\n}}}")
          debug.println("}}}")

          MutRes(heap, mrvalue(AV(AP.Constant -> LabelSet.empty)), Map.empty)

        // TODO array mutations have their own magick symbol-less methods

        case tree @ Apply(sel @ Select(obj, _), args) if sel.symbol.isSetter && sel.symbol.is(Flags.Mutable) =>
          val fieldName = sel.symbol.name.asTermName.getterName
          val d = sel.symbol.owner.info.decl(fieldName)

          val arg :: Nil = args // expecting only one arg for a setter
          val abstractArg = _analyze(arg)

          val escapingLocals =
            store.keys.filter { sym => abstractArg.self.contains(AP(sym)) }.toList

          if (escapingLocals.nonEmpty) {
            ctx.error(i"Locals escaping through assignment: $escapingLocals%, %", tree.sourcePos)
          }

          val res =
            (for {
              (ap, ls) <- _analyze(obj).iterator // TODO evaluating obj /can/ produce side-effects
              if ls.isEmpty // TODO only the strongs must be empty, find an error case
            } yield ap).foldLeft(heap) { (heap, ap) =>
              heap.updatedWith(ap, d.symbol, abstractArg) { av =>
                av merge abstractArg
              }
            }

          MutRes(heap, mrvalue(AV(AP.Constant -> LabelSet.empty)), Map.empty)

        case tree @ Apply(fun, args) =>

          def fallbackResult =
            MutRes(heap, mrvalue(_analyze(tree, heap)), Map.empty)

          val maybeFunDef = fun.symbol.defTree

          if (maybeFunDef.isEmpty) {
            debug.println(i"#!apply-empty")
            fallbackResult
          } else if (fun.isInstanceOf[Select]) {
            debug.println(i"#!apply-method")
            fallbackResult
          } else {
            val funDef = maybeFunDef.asInstanceOf[DefDef]
            val (newStore: Store, _) = analyseArgsIntoNewStore(args, funDef.vparamss.head)
            loop(funDef.rhs, _store = newStore, _terminal = terminal)
          }

        case If(cond, thenp, elsep) =>
          val res1 = loop(cond)
          val done = res1.value == MRValue.Abort

          // TODO test the code for exiting from if-conditions
          if (done) res1 else {
            val heap1 = res1.heap
            merge(
              loop(thenp, _heap = heap, _terminal = terminal),
              loop(elsep, _heap = heap, _terminal = terminal)
            )
          }

        case Return(expr, from) =>
          debug.println(i"#!return {{{")
          debug.println(i"#expr {${expr.productPrefix}} {{{\n${expr}\n}}}")
          debug.println(i"#from {${from.productPrefix}} {{{\n${from}\n}}}")
          debug.println("}}}")

          // TODO: analyse the expr for terrible nested returns
          MutRes(heap, MRValue.Abort, Map(from.symbol.name -> _analyze(expr, _heap = heap)))

        case Labeled(bind, tree) =>
          debug.println(i"#!labeled {{{")
          debug.println(i"#bind {${bind.productPrefix}} {{{\n${bind}\n}}}")
          debug.println("}}}")

          val res1 = loop(tree, _terminal = terminal)

          val blockLabel = bind.symbol.name
          val returnedAV =
            res1.returns.getOrElse(blockLabel, {
              // TODO ASK: is it ever possible to have a Labeled that has no associated returns?
              debug.println("#!!!labeled-no-returns")
              AV.Empty
            })

          def result(av: AV) =
            res1.copy(
              value = MRValue.Proper(av),
              returns = res1.returns - blockLabel
            )


          res1.value match {
            case MRValue.Abort =>
              result(returnedAV)
            case MRValue.Proper(av) =>
              // TODO ASK: it is ever possible for a labeled block to actually evaluate to a value?
              result(av merge returnedAV)
            case MRValue.Skipped => sys.error("terminal accumulation returned a MRValue.skipped")
          }

        case Block(stmts, expr) =>
          var res: MutRes = null
          val iter = stmts.iterator
          var done: Boolean = false
          while (!done && iter.hasNext) {
            res =
              if (res == null)
                loop(iter.next)
              else
                integrate(res, iter.next)
            done = res.value == MRValue.Abort
          }

          if (!done) {
            res =
              if (res == null)
                loop(expr, _terminal = terminal)
              else
                integrate(res, expr, _terminal = terminal)
          }

          res

        case tree =>
          MutRes(heap, mrvalue(_analyze(tree)), Map.empty)
      }
    }
  }

  def analyseToplevel(tree: Tree)(implicit ctx: Context): AV =
    analyse(tree, Map.empty, Map.empty, Heap.Empty)

  def analyse(
    tree: Tree,
    store: Store,
    cache: Cache,
    heap: Heap
  )(implicit ctx: Context): AV = {
    def loop(
      tree: Tree,
      _store: Store = store,
      _cache: Cache = cache,
      _heap: Heap = heap
    )(implicit ctx: Context) = analyse(tree, _store, _cache, _heap)

    val uncluttered = unclutter(tree, dropBlocks = true)

    def analyseArgsIntoNewStore(
      args: List[Tree],
      params: List[Tree],
      _store: Store = store
    ): (Store, List[AV]) = {
      var res: Store = _store

      val abstractArgsBld = ListBuffer.empty[AV]
      for {
        (vp, arg) <- params lazyZip args // TODO assuming that closures/constructors have exactly one parameter list
      } do {
        val abstractArg = loop(arg)
        abstractArgsBld += abstractArg
        res = res.updated(vp.symbol, abstractArg)
      }

      (res, abstractArgsBld.result)
    }

    inline def retrieve0(
      ap: AP,
      abstractArgs: List[AV],
      onMiss: Cache => AV,
      onError: => AV
    ): AV =
      cache.get((ap, abstractArgs)) match {
        case Some(av: AV) => av
        case Some("err") => onError
        case None =>
          onMiss(cache.updated((ap, abstractArgs), "err"))
      }

    inline def retrieve(
      ap: AP,
      abstractArgs: List[AV],
      newStore: Store,
      body: Tree,
      onError: => AV
    ): AV =
      retrieve0(ap, abstractArgs, loop(body, newStore, _), onError)

    trace(i"analyse(${tersely(uncluttered)}; ${store.keys.map(_.name).toList}%, %)", debug, showResult) {
      debug.println("{{{")
      debug.println(i"#tree = {{{\n${uncluttered}\n}}}")
      debug.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      debug.println(s"#heap = {{{\n${Heap.display(heap)}\n}}}")
      debug.println("}}}")

      uncluttered match {

        case tpd.Literal(_) =>
          AV(Map(AP.Constant -> LabelSet.empty))

        case tree @ Apply(sel @ Select(obj, _), Nil) if sel.symbol.isGetter && sel.symbol.is(Flags.Mutable) =>
          val sym = sel.symbol.underlyingSymbol
          debug.println(i"#sel.symbol.## ${sel.symbol.##}")
          debug.println(i"#sel.symbol.name.## ${sel.symbol.name.##}")
          debug.println(i"#!sym.flags.flagsString ${sym.flags.flagsString}")

          var res = AV.Empty
          for {
            (ap, ls) <- loop(obj).iterator
            if ls.isEmpty // TODO only the strongs must be empty, find a test case
          } do {
            res = res.merge(heap.getOrElse(ap, sym, {
              debug.println(i"#!!! Mutable symbol absent from heap! AP: ${ap.display} ; sym: ${sym}")
              AV.Empty
            }))
          }

          res

        case This(_) =>
          store.get(thisStoreKey) match {
            case None =>
              debug.println(i"#!!! @This tree, `this` absent from store!")
              AV.Empty // TODO AnyValue?
            case Some(av) => av
          }

        case tree @ Select(This(_), name) =>
          // assuming that we have a member
          debug.println(i"#member-symbol-select {name=${name}#${name.##}}")

          store.get(thisStoreKey) match {
            case None =>
              debug.println(i"#!!! Tried to look up $name from the store, but `this` is absent")
              AV.Empty // TODO AnyValue?
            case Some(av) =>
              av.filter { case (_, value) => value.weak.contains(name) }
          }

        case tree @ Ident(name) if tree.symbol.owner.isClass =>
          // assuming that we have a member
          debug.println("#member-symbol-ident")
          store.get(thisStoreKey) match {
            case None =>
              debug.println(i"!!! Tried to look up $name from the store, but `this` is absent")
              AV.Empty // TODO AnyValue?
            case Some(av) =>
              av.filter { case (_, value) => value.weak.contains(name) }
          }

        case tree @ Ident(_) =>
          debug.println(i"#ident-data {{{")
          debug.println(i"#tree.name {{{\n${tree.name}\n}}}")
          debug.println(i"#tree.qualifier {{{\n${tree.qualifier}\n}}}")
          debug.println("}}}")

          store.getOrElse(tree.symbol,
            tree.symbol.defTree match {
              case EmptyTree => sys.error(i"missing def tree for {${tree.symbol}} shouldn't happen, missing -Yretain-trees")
              case dt: ValDef =>
                debug.println(i"going into def tree of $tree")
                accumulate(
                  dt.rhs,
                  store = store,
                  cache = cache,
                  heap = heap,
                  terminal = true
                ).value.expected_!
            }
          )

        case tree @ If(_, thenp, elsep) =>
          loop(thenp) merge loop(elsep)

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

          def constructorAssignmentsIter: Iterator[(AP , LabelSet)] = {
            val newStore = {
              var res = store
              for {
                (p, av) <- constructorArgPairs
              } do {
                res = res.updated(p.symbol, av)
              }
              res
            }

            debug.println("#constructor-iteration {{{")
            debug.println(i"#constructorDef.rhs (${constructorDef.rhs.productPrefix}) {{{\n${constructorDef.rhs}\n}}}")
            val collected = ArrayBuilder.make[Iterator[(AP , LabelSet)]]
            def iterate(tree: Tree): Unit =
              tree match {
                case Block(stmts, expr) =>
                  stmts.foreach(iterate)
                  iterate(expr)
                case Assign(Select(This(_), name), rhs) =>
                  collected += loop(rhs, newStore).iterator.map {
                    case (k, v) => k -> (v + name)
                  }
                case Assign(lhs, rhs) =>
                  debug.println(i"#assign:")
                  debug.println(i"##lhs (${lhs.productPrefix}) {{{\n${lhs}\n}}}")
                  debug.println(i"##rhs (${rhs.productPrefix}) {{{\n${rhs}\n}}}")
                case tree =>
                  debug.println(i"#seeing: {${tree.productPrefix}} {{{\n${tree}\n}}}")
              }
            iterate(constructorDef.rhs)
            debug.println("}}}")

            collected.result.iterator.flatten
          }

          assert(args.hasSameLengthAs(constructorVparams))
          AV.merge(
            Iterator(AP(new_) -> LabelSet.empty)
              ++ constructorAssignmentsIter
              ++ {
                for {
                  (param, av) <- constructorArgPairs
                  (key, labelSet) <- av.iterator
                } yield key -> (labelSet + param.name)
              }
          )

        /// method call
        case tree @ Apply(sel @ Select(objTree, ident), args)
            if !objTree.symbol.is(Flags.Module) =>
          debug.println(i">method-call")

          def analyseMethodCall(ap: AP, abstractObj: AV): AV =
            (ap: @unchecked) match {
              case AP.Tree(clos @ Closure(env, closRef, _)) =>
                // TODO: test the case when the method cannot be statically determined
                val closDef = closRef.symbol.defTree.asInstanceOf[DefDef]

                val (newStore1: Store, abstractArgs: List[AV]) =
                  analyseArgsIntoNewStore(args, closDef.vparamss.head.drop(env.length))

                val newStore0 = {
                  val envSyms = closDef.vparamss.head.take(env.length).map(_.symbol)
                  abstractObj.iterator.foldLeft(newStore1) { (store, kv) =>
                    val (ap, ls) = kv
                    envSyms.find(s => ls.weak.contains(s.name)) match {
                      case Some(sym) =>
                        store.updated(sym, AV(ap -> ls))
                      case None =>
                        store
                    }
                  }
                }

                retrieve(ap, abstractArgs, newStore0, closDef.rhs, onError = {
                  debug.println(i"#! Ran out of abstract stack when analysing a closure: {{{")
                  debug.println(i"#clos {{{\n${clos}\n}}}")
                  debug.println("}}}")
                  AV.Empty
                })

              case AP.Tree(New(cls)) =>
                val methDef = sel.symbol.defTree.asInstanceOf[DefDef]
                val cstrDef = cls.symbol.primaryConstructor.defTree.asInstanceOf[DefDef]
                debug.println(i"#method-def-tree {${ident.toString}} {{{")
                debug.println(i"#cstrDef {{{\n${cstrDef}\n}}}")
                debug.println(i"#methDef {{{\n${methDef}\n}}}")
                debug.println("}}}")

                val (newStore1: Store, abstractArgs: List[AV]) =
                  analyseArgsIntoNewStore(args, methDef.vparamss.head)

                val newStore0 = newStore1.updated(thisStoreKey, abstractObj)

                retrieve(ap, abstractArgs, newStore0, methDef.rhs, onError = {
                  debug.println(i"#! Ran out of abstract stack when analysing a method: (${sel.symbol.name}) {{{")
                  debug.println(i"#methDef.rhs {{{\n${methDef.rhs}\n}}}")
                  debug.println("}}}")
                  AV.Empty
                })

              case AP.Sym(sym) =>
                // What does it /mean/ when we have a symbol in the map?
                // It means we are dealing with a local variable.
                // Therefore, we should first check if we can get a signature of the method being selected
                // or otherwise we assume the worst
                // should probably deal with the easy case first, where we're calling .apply on lambda we have signature of
                val info = ctx.atPhase(ctx.postTyperPhase) { sym.denot.info }
                if (ident == nme.apply && defn.isFunctionType(info)) {
                  // TODO assuming that closures/constructors have exactly one parameter list
                  val AppliedType(_, params) = info // assuming that a "function type" is always an applied type
                  var res = AV.Empty
                  for {
                    (param, arg) <- params lazyZip args // discards the result type parameter
                    if !param.hasAnnotation(defn.LocalAnnot)
                  } do {
                    res = res merge loop(arg)
                  }

                  res
                } else {
                  debug.println(i"#!sym-method-call")
                  // TODO retrieve signatures for arbitrary methods
                  args.foldLeft(AV(ap -> LabelSet.empty)) {
                    (av, arg) => av merge loop(arg)
                  }
                }

              case AP.Constant =>
                args.foldLeft(AV(AP.Constant -> LabelSet.empty)) {
                  (av, arg) => av merge loop(arg)
                }
            }


          val abstractObj = loop(objTree, store)
          var res = AV.Empty
          for {
            (ap, emptyLabels) <- abstractObj.iterator
            if emptyLabels.isEmpty // TODO only the /strong/ set should be empty, find an error case for this
            // TODO if types overlap (why types need to overlap? does it make more sense to check if ident exists on ap?)
          } do {
            res = res merge analyseMethodCall(ap, abstractObj)
          }

          res

        case tree @ Apply(fun, args) if !fun.symbol.defTree.isEmpty =>

          val fsym = fun.symbol
          val funDef = fsym.defTree.asInstanceOf[DefDef] // StoreAnnotations phase only adds this annot to DefDefs

          debug.println(i"#function-call {{{")
          debug.println(i"#funDef {${funDef.productPrefix}} {{{\n${funDef}\n}}}")
          debug.println("}}}")

          val rparams = funDef.vparamss.head.map { p =>
            p.symbol.getAnnotation(ctx.definitions.LocalParamsAnnot) match {
              case Some(annot) =>
                val Apply(_, Literal(Constants.Constant(c)) :: Nil) = annot.tree
                val arg = c.asInstanceOf[String] // unwrap the constant
                arg
              case None =>
                ""
            }
          }

          if fsym.hasAnnotation(ctx.definitions.LocalParamsAnnot) then {
            // Analyse arguments that are expected to define lambdas with local parameters
            debug.println(i"rparams of $fsym")
            (args zip rparams).zipWithIndex foreach {
              case ((arg, spec), idx) => spec match {
                case "" => ;
                case _ =>
                  // local param indices
                  val lpIndices = spec.split(",").iterator.map(_.toInt).toSet

                  val Block(_, Block(_, clos @ Closure(env, meth, _))) = arg // TODO ?!?

                  debug.println(i"#preparing-local-analysis: ${clos.##}")

                  val underlyingDefDef = meth.symbol.defTree.asInstanceOf[DefDef]
                  debug.println(i"#<${meth.symbol}> {{{")
                  debug.println(i"#defTree =\n$underlyingDefDef")
                  debug.println(i"#env = [${env.length}] $env%, %")
                  debug.println("}}}")
                  // TODO assuming that closures/constructors have exactly one parameter list
                  // TODO inefficient length call
                  val closureParams = underlyingDefDef.vparamss.head.drop(env.length)
                  val closureEnvParams = underlyingDefDef.vparamss.head.take(env.length)

                  val (closureLocalParams: LocalParams, newStore: Store) = {
                    var res1: LocalParams = SimpleIdentitySet.empty
                    var res2: Store = store
                    closureParams.iterator.zipWithIndex.foreach { case (p, idx) =>
                      if lpIndices.contains(idx) then res1 += p.symbol
                      res2 = res2.updated(p.symbol, AV(AP(p.symbol) -> LabelSet.empty))
                    }
                    for (p, t) <- closureEnvParams lazyZip env do {
                      debug.println(i"#env-param ${p.name} → {{{\n${t}\n}}}")
                      res2 = res2.updated(p.symbol, loop(t))
                    }
                    (res1, res2)
                  }

                  val finalExpr = finalExprOf(underlyingDefDef.rhs)

                  debug.println(i"#starting-local-analysis: ${clos.##}")

                  val analysed: AV = loop(finalExpr, newStore)

                  val escapees =
                    analysed.self.keysIterator.filter(ap => closureLocalParams.contains(ap.maybeSym)).toSeq
                  if (escapees.size > 0) {
                    ctx.error(i"(c) escaping locals: ${escapees}%, %", finalExpr.sourcePos)
                    ctx.reporter.flush()
                  } else {
                    debug.println("(c) no locals escape")
                  }
              }
            }
          }

          val (newStore: Store, abstractArgs: List[AV]) =
            analyseArgsIntoNewStore(args, funDef.vparamss.head)

          retrieve0(AP(fun.symbol), abstractArgs,
            onMiss = { cache =>
              accumulate(
                tree = funDef.rhs,
                cache = cache,
                store = newStore,
                heap = heap,
                terminal = true
              ).value.asInstanceOf[MRValue.Proper].value
            },
            onError = {
              debug.println(i"#! Run out of abstract stack when interpreting ${fun.symbol}")
              AV.Empty // NOTE! recursion-termination
            }
          )

        case tree @ Closure(env, meth, _) =>
          val closureDefTree = meth.symbol.defTree.asInstanceOf[DefDef]
          debug.println(i"#closure {{{")
          debug.println(i"#tree ${tree}")
          debug.println(i"#closureDefTree {${closureDefTree.productPrefix}} {{{\n${closureDefTree}\n}}}")
          debug.println(i"#env $env%, %")
          debug.println("}}}")

          var res = AV(AP(tree) -> LabelSet.empty)
          for {
            (t, vt) <- env lazyZip closureDefTree.vparamss.head
            (ap, ls) <- loop(t).iterator
          } do {
            res = res.merge(ap, ls + vt.symbol.name)
          }

          res

        case tree =>
          debug.println(i"#!!! UNRECOGNIZED TREE: empty abstract value for ${tree.getClass.getSimpleName}:{{{\n${ tree }\n${ tree.toString }\n}}}")
          AV.Empty
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
        res2 = res2.updated(vp.symbol, AV(AP(vp.symbol) -> LabelSet.empty))

      (res1, res2)
    }

    if (localParams.size != 0)
      debug.println(i"local params for ${tree.symbol} ==> ${localParams.toList}%, %")

    val finalExpr = finalExprOf(tree.rhs)

    // import given Flags.FlagOps
    if (localParams.size > 0) {
      debug.println(i"${tree.name}: detected local params")
      val mutRes = accumulate(tree.rhs, localStore.toMap, Map.empty, Heap.Empty, terminal = true)
      val av = mutRes.value match {
        case MRValue.Proper(av) =>
          debug.println("#mut-res: proper")
          av
        case MRValue.Abort =>
          debug.println("#mut-res: abort")
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
            ctx.error(i"(m) local escapes ($sym)", sym.sourcePos)
            ctx.reporter.flush()
        }}
      } else {
        debug.println("No locals escape")
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
    case Sym(symbol: Symbol)
    case Tree(tree: tpd.Tree)
    case Constant

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
        case Constant => Str("Constant")
      }

      Str("AP(") ~ txt ~ Str(")")
    }

    def display(implicit ctx: Context) =
      this match {
        case AP.Sym(s) => s.name
        case AP.Tree(t) => s"${tersely(t)}#${t.##}"
        case AP.Constant => "<Constant>"
      }
  }

  object AP {
    def apply(symbol: Symbol) = AP.Sym(symbol)
    def apply(tree: tpd.Tree) = AP.Tree(tree)
  }

  class AV(val self: Map[AP, LabelSet]) extends AnyVal {
    def iterator = self.iterator

    def filter(f: ((AP, LabelSet)) => Boolean): AV =
      AV(this.self.filter(f))

    def merge(ap: AP, ls1: LabelSet): AV =
      AV(this.self.updatedWith(ap) {
        case None => Some(ls1)
        case Some(ls2) =>
          Some(LabelSet.merge(ls1, ls2))
      })

    def merge(other: AV): AV =
      if (other.self.size < this.self.size)
        merge(other.iterator)
      else
        other.merge(this.iterator)

    def merge(other: Iterator[(AP, LabelSet)]): AV = {
      var res = this
      for {
        (ap, ls1) <- other
      } do {
        res = res.merge(ap, ls1)
      }
      res
    }
  }

  object AV {
    val Empty = AV(Map.empty)

    def apply(kv: (AP, LabelSet)): AV = AV(Map(kv))
    def apply(map: Map[AP, LabelSet]): AV = new AV(map)

    def resolve(av: AV, heap: Heap): AV = {
      merge(
        av.iterator
          ++ (for {
            (ap, ls) <- av.iterator
            (sym, av1) <- heap.self.get(ap).iterator.flatten
          } yield av1.iterator.map { case (ap1, ls1) =>
              ap1 -> LabelSet(
                weak = ls.weak union ls1.weak,
                strong = ls.strong union ls1.strong
              )
          }).flatten
      )
    }

    def merge(labels: Iterator[(AP, LabelSet)]): AV = {
      val resIter = for {
        (a, g) <- labels.toArray.groupBy(_._1).iterator
      } yield a -> (g.map(_._2).reduce(LabelSet.merge))
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

      for (k, v) <- av.iterator
      do
        nl()
        res append k.display
        res append s" ← ${v.display}"

      res.toString
    }
  }

  type Cache = Map[(AP, List[AV]), AV | "err"]

  enum MRValue {
    def expected_! = this match {
      case Proper(av) => av
      case _ => sys.error("expected MRValue to be proper!")
    }

    case Skipped;
    case Abort;
    case Proper(value: AV);
  }

  case class MutRes(heap: Heap, value: MRValue, returns: Map[Name, AV])

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
        val ln = s"${ap.display}.${sym.name} → "
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
}
