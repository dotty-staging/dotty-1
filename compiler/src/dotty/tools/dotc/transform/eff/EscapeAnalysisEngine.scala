package dotty.tools
package dotc
package transform
package eff

import core._
import Decorators._
import Symbols._
import Types._

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
  val `scala.Long.unbox` = ctx.requiredModule("scala.Long").requiredMethod("unbox")
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
      case r: MutRes => r match {
        case MutRes.NoReturn(h) =>
          s"{{{\n#NoReturn\n#heap {{{\n${Heap.display(h)}\n}}}\n}}}"
        case MutRes.Return(h, av, t) =>
          val res = new StringBuilder
          res append "{{{\n"
          res append "#Return(total = $t)\n"
          res append "#heap {{{\n${Heap.display(h)}\n}}}\n"
          res append "#av {{{\n${AV.display(av)}\n}}}\n"
          res append "}}}"
          res.toString
      }
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

    def integrate(res: MutRes, tree: Tree, _terminal: Boolean = false) = {
      res match {
        case MutRes.NoReturn(heap) =>
          loop(tree, _heap = heap, _terminal = _terminal)
        case MutRes.Return(heap, res1, total) =>
          if (total) res else {
            loop(tree, _heap = heap, _terminal = _terminal) match {
              case MutRes.NoReturn(heap) =>
                MutRes.Return(heap, res1, total)
              case MutRes.Return(heap, res2, total) =>
                MutRes.Return(
                  heap,
                  AV.merge(res1.iterator ++ res2.iterator),
                  total
                )
            }
          }
      }
    }

    /** Merge mut-results from two _branching_ codepaths. */
    def merge(resA: MutRes, resB: MutRes): MutRes =
      (resA, resB) match {
        case (MutRes.NoReturn(heap1), MutRes.NoReturn(heap2)) =>
          MutRes.NoReturn(heap1 merge heap2)
        case (MutRes.Return(heap1, av, total), MutRes.NoReturn(heap2)) =>
          MutRes.Return(heap1 merge heap2, av, total)
        case (MutRes.NoReturn(heap1), MutRes.Return(heap2, av, total)) =>
          MutRes.Return(heap1 merge heap2, av, total)
        case (MutRes.Return(heap1, av1, total1), MutRes.Return(heap2, av2, total2)) =>
          MutRes.Return(
            heap1 merge heap2,
            av1 merge av2,
            total1 && total2
          )
      }

    def _analyse(tree: Tree, _heap: Heap = heap)(implicit ctx: Context) = analyse(tree, store, cache, _heap)

    val uncluttered = unclutter(tree, dropBlocks = false)
    trace(i"accumulate(${tersely(uncluttered)}; terminal=$terminal; ${store.keys.map(_.name).toList}%, %)", debug, showHeap) {

      debug.println("{{{")
      debug.println(i"#uncluttered = {{{\n${uncluttered}\n}}}")
      debug.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      debug.println(i"#tree = {{{\n${tree}\n}}}")
      debug.println("}}}")

      uncluttered match {
        /// constructor
        case tree @ Apply(Select(_, ident), _) if ident == nme.CONSTRUCTOR =>
          // ! SSA-assumption

          // just delegate to analyse
          if (!terminal)
            MutRes.NoReturn(heap)
          else
            MutRes.Return(heap, _analyse(tree), total = true)

        case Assign(lhs, rhs) =>
          debug.println(i"#!acc-assign {{{")
          debug.println(i"#lhs {${lhs.productPrefix}} {{{\n${lhs}\n}}}")
          debug.println(i"#rhs {${rhs.productPrefix}} {{{\n${rhs}\n}}}")
          debug.println("}}}")

          assert(!terminal)
          MutRes.NoReturn(heap)

        // TODO array mutations have their own magick symbol-less methods

        case Apply(sel @ Select(obj, _), args) if sel.symbol.isSetter && sel.symbol.is(Flags.Mutable) =>
          val fieldName = sel.symbol.name.asTermName.getterName
          val d = sel.symbol.owner.info.decl(fieldName)

          val arg :: Nil = args // expecting only one arg for a setter
          val abstractArg = _analyse(arg)

          val res =
            (for {
              (ap, ls) <- _analyse(obj).iterator // TODO evaluating obj /can/ produce side-effects
              if ls.isEmpty // TODO only the strongs must be empty, find an error case
            } yield ap).foldLeft(heap) { (heap, ap) =>
              heap.updatedWith(ap, d.symbol, abstractArg) { av =>
                av merge abstractArg
              }

            }

          assert(!terminal)
          MutRes.NoReturn(res)

        case tree @ Apply(meth, args) =>
          debug.println(i"#!acc-apply {{{")
          debug.println(i"#meth {${meth.productPrefix}} {{{\n${meth}\n}}}")
          debug.println(i"#meth.toString ${meth.toString}")
          debug.println(i"#meth.symbol ${meth.symbol}")
          for (arg, i) <- args.iterator.zipWithIndex
          do debug.println(i"#args[$i] ${arg}")
          debug.println("}}}")

          if (!terminal)
            MutRes.NoReturn(heap)
          else
            MutRes.Return(heap, _analyse(tree, heap), total = true)

        case If(cond, thenp, elsep) =>
          val res1 = loop(cond)
          val done = res1 match {
            case _: MutRes.NoReturn => false
            case MutRes.Return(_, _, total) => total
          }

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
          MutRes.Return(heap, _analyse(expr, _heap = heap), total = true)

        case Labeled(bind, tree) =>
          debug.println(i"#!labeled {{{")
          debug.println(i"#bind {${bind.productPrefix}} {{{\n${bind}\n}}}")
          debug.println("}}}")

          loop(tree, _terminal = terminal)

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
            done = res.isInstanceOf[MutRes.Return]
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
          if (!terminal)
            MutRes.NoReturn(heap)
          else
            MutRes.Return(heap, _analyse(tree), total = true)
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
  )(implicit ctx: Context): AV =
    def loop(
      tree: Tree,
      _store: Store = store,
      _cache: Cache = cache,
      _heap: Heap = heap
    )(implicit ctx: Context) = analyse(tree, _store, _cache, _heap)

    val uncluttered = unclutter(tree, dropBlocks = true)

    trace(i"analyse(${tersely(uncluttered)}; ${store.keys.map(_.name).toList}%, %)", debug, showResult) {
      debug.println("{{{")
      debug.println(i"#uncluttered = {{{\n${uncluttered}\n}}}")
      debug.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      debug.println(s"#heap = {{{\n${Heap.display(heap)}\n}}}")
      debug.println(i"#tree = {{{\n${tree}\n}}}")
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
                loop(dt.rhs)
            }
          )

        case tree @ If(_, thenp, elsep) =>
          AV.merge(
            loop(thenp).iterator ++ loop(elsep).iterator
          )

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
                  debug.println(i"#bingo: $name#${name.##}")
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
              case AP.Tree(cls @ Closure(env, meth, _)) =>
                // TODO: test the case when the method cannot be statically determined
                val methDef = meth.symbol.defTree.asInstanceOf[DefDef]
                val closureBody = methDef.rhs

                val (newStore: Store, abstractArgs: List[AV]) = {
                  var res: Store = store

                  val abstractArgsBld = ListBuffer.empty[AV]
                  val params = methDef.vparamss.head.drop(env.length)
                  for {
                    (vp, arg) <- params lazyZip args // TODO assuming that closures/constructors have exactly one parameter list
                  } do {
                    val abstractArg = loop(arg)
                    abstractArgsBld += abstractArg
                    res = res.updated(vp.symbol, abstractArg)
                  }

                  val envSyms = methDef.vparamss.head.take(env.length).map(_.symbol)
                  for {
                    (ap, ls) <- abstractObj.iterator
                    sym <- envSyms.find(s => ls.weak.contains(s.name))
                  } do {
                    res = res.updated(sym, AV(ap -> ls))
                  }

                  (res, abstractArgsBld.result)
                }

                cache.get((ap, abstractArgs)) match {
                  case Some(av: AV) => av
                  case Some("err") =>
                    debug.println(i"#! Ran out of abstract stack when analysing a closure: {{{")
                    debug.println(i"#cls {{{\n${cls}\n}}}")
                    // for {
                    //   (aarg, i) <- abstractArgs.iterator.zipWithIndex
                    // } do {
                    //   debug.println(i"#aarg[$i] {{{\n${aarg}\n}}}")
                    // }
                    debug.println("}}}")
                    AV.Empty
                  case None =>
                    loop(closureBody, newStore, cache.updated((ap, abstractArgs), "err"))
                }

              case AP.Tree(New(cls)) =>
                val methDef = sel.symbol.defTree.asInstanceOf[DefDef]
                val cstrDef = cls.symbol.primaryConstructor.defTree.asInstanceOf[DefDef]
                debug.println(i"#method-def-tree {${ident.toString}} {{{")
                debug.println(i"#cstrDef {{{\n${cstrDef}\n}}}")
                debug.println(i"#methDef {{{\n${methDef}\n}}}")
                debug.println("}}}")

                val (newStore: Store, abstractArgs: List[AV]) = {
                  var res: Store = store

                  val abstractArgsBld = ListBuffer.empty[AV]
                  val methParams = methDef.vparamss.head
                  for
                    (vp, arg) <- methParams lazyZip args // TODO assuming that closures/constructors have exactly one parameter list
                  do {
                    val abstractArg = loop(arg)
                    abstractArgsBld += abstractArg
                    res = res.updated(vp.symbol, abstractArg)
                  }

                  res = res.updated(thisStoreKey, abstractObj)
                  (res, abstractArgsBld.result)
                }

                cache.get((ap, abstractArgs)) match {
                  case Some(av: AV) =>
                    av
                  case Some("err") =>
                    debug.println(i"#! Ran out of abstract stack when analysing a method: (${sel.symbol.name}) {{{")
                    debug.println(i"#methDef.rhs {{{\n${methDef.rhs}\n}}}")
                    // for {
                    //   (aarg, i) <- abstractArgs.iterator.zipWithIndex
                    // } do {
                    //   debug.println(i"#aarg[$i] {{{\n${aarg}\n}}}")
                    // }
                    debug.println("}}}")
                    AV.Empty // TODO: this should be /all/ possible values
                  case None =>
                    loop(methDef.rhs, newStore, cache.updated((ap, abstractArgs), "err"))
                }

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
                // AV.merge(
                //   args.iterator.map(loop(_)).flatten
                //     ++ Iterator(AP.Constant -> LabelSet.empty)
                // )
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
          val symTree = fsym.defTree.asInstanceOf[DefDef] // StoreAnnotations phase only adds this annot to DefDefs

          debug.println(i"#function-call {{{")
          debug.println(i"#symTree {${symTree.productPrefix}} {{{\n${symTree}\n}}}")
          debug.println("}}}")

          val rparams = symTree.vparamss.head.map { p =>
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
                  } else {
                    debug.println("(c) no locals escape")
                  }
              }
            }
          }

          val newStore: Store =
            var res: Store = store
            for
              (vp, arg) <- symTree.vparamss.head lazyZip args // TODO assuming that closures/constructors have exactly one parameter list
            do
              res = res.updated(vp.symbol, loop(arg))
            res

          // TODO !!! NEED TO RUN EFFECT ANALYSIS HERE!
          loop(symTree.rhs, newStore)

        case tree @ Closure(env, meth, _) =>
          val closureDefTree = meth.symbol.defTree.asInstanceOf[DefDef]
          debug.println(i"#closure {{{")
          debug.println(i"#tree ${tree}")
          debug.println(i"#closureDefTree {${closureDefTree.productPrefix}} {{{\n${closureDefTree}\n}}}")
          debug.println(i"#env $env%, %")
          debug.println("}}}")


          /**
            * Represents a user-inaccessible name and marks values that were
            * captured and can be returned from an arbitrary method.
            */
          val capturedValueMarker = nme.EMPTY
          AV.merge(
            Iterator(AP(tree) -> LabelSet.empty)
              ++ (env lazyZip closureDefTree.vparamss.head).iterator.map { case (t, vt) =>
                loop(t).iterator.map {
                  case (ap, lbls) => ap -> LabelSet(
                    lbls.weak + capturedValueMarker + vt.symbol.name,
                    lbls.strong + capturedValueMarker + vt.symbol.name)
                }
              }.flatten
          )

        case tree =>
          debug.println(i"#!!! UNRECOGNIZED TREE: empty abstract value for ${tree.getClass.getSimpleName}:{{{\n${ tree }\n${ tree.toString }\n}}}")
          AV.Empty
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
      val (heap, av) = mutRes match {
        case MutRes.NoReturn(heap) =>
          debug.println("#mut-res-no-return")
          // val analysed = analyse(finalExpr, localStore.toMap, Map.empty, heap)
          (heap, AV.Empty)
        case MutRes.Return(heap, res, total) =>
          debug.println("#mut-res-return")
          assert(total)
          (heap, res)
      }

      val resolved = AV.resolve(av, heap)
      val escapees =
        resolved.self.keysIterator.filter(ap => localParams.contains(ap.maybeSym)).toSeq
      if (escapees.size > 0) {
        escapees.foreach { s => (s: @unchecked) match {
          case AP.Sym(sym) =>
            ctx.error(i"(m) local escapes ($sym)", sym.sourcePos)
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
          if tree.symbol == `scala.Long.unbox`
        || tree.symbol == `scala.Int.int2long`
          => expr
      case TypeApply(expr, _) => loop(expr)
      case Select(expr, ident) if ident == nme.asInstanceOf_ => expr
      case expr => expr
    }
  }

  // def peek[S <: Showable](str: String, peeked: S): S = {
  //   debug.println(i"$str: $peeked # ${peeked.toString}")
  //   peeked
  // }
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

  // TODO this should probably be removed
  type LocalParams = SimpleIdentitySet[Symbol]

  enum AP extends Showable {
    case Sym(symbol: Symbol)
    case Tree(tree: tpd.Tree)
    case Constant

    // NOTE hashCode should work out OK, since all union members have it implemented

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

    def merge(other: AV): AV =
      if (other.self.size < this.self.size)
        merge(other.iterator)
      else
        other.merge(this.iterator)

    def merge(other: Iterator[(AP, LabelSet)]): AV = {
      var res = this.self
      for {
        (ap, ls1) <- other
      } do {
        res = res.updatedWith(ap) {
          case None => Some(ls1)
          case Some(ls2) =>
            Some(LabelSet.merge(ls1, ls2))
        }
      }
      AV(res)
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
        if nl_? then res append "\n"
        nl_? = true

      for (k, v) <- av.iterator
      do
        nl()
        res append spaces
        res append k.display
        res append s" ← ${v.display}"

      res.toString
    }
  }

  type Cache = Map[(AP, List[AV]), AV | "err"]

  trait HasHeap {
    def heap: Heap
  }

  enum MutRes extends HasHeap{
    case NoReturn(heap: Heap);
    case Return(heap: Heap, result: AV, total: Boolean);
  }

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
                case Some(av2) => Some(AV.merge(av1.iterator ++ av2.iterator))
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
        val ln = s"${ap.display}.${sym.name} →"
        buf append ln
        buf append AV.display(v, ln.length + 1).drop(ln.length)
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
        val ln = s"${spaces}${k.name} →"
        buf append ln
        buf append AV.display(v, ln.length + 1).drop(ln.length)
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
