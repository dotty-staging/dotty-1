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
import config.Printers.debug

import printing.{Showable, Printer}
import printing.Texts.Text

import transform.MegaPhase.MiniPhase

import StdNames.nme

import reporting.trace

import util.SimpleIdentitySet
import scala.collection.mutable.{ArrayBuilder, ListBuffer}

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
    it match
    case r: (Heap @unchecked) => s"{{{\n${Heap.display(r)}\n}}}"
    case _ => s"{{{\n${ it }\n}}}"
  }

  def accumulate(
    tree: Tree,
    store: Store = Map.empty,
    cache: Cache = Map.empty,
    heap: Heap = Heap.Empty
  )(implicit ctx: Context): Heap = {
    def loop(
      tree: Tree,
      _store: Store = store,
      _cache: Cache = cache,
      _heap: Heap = heap
    )(implicit ctx: Context) = accumulate(tree, _store, _cache, _heap)

    def _analyse(tree: Tree)(implicit ctx: Context) = analyse(tree, store, cache, heap)

    val uncluttered = unclutter(tree, dropBlocks = false)
    trace(i"accumulate(${tersely(uncluttered)}; ${store.keys.map(_.name).toList}%, %)", debug, showHeap) {

      debug.println("{{{")
      debug.println(i"#uncluttered = {{{\n${uncluttered}\n}}}")
      debug.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      debug.println(i"#tree = {{{\n${tree}\n}}}")
      debug.println("}}}")

      uncluttered match {
        case Block(stmts, expr) =>
          val heap1 = stmts.foldLeft(heap) { (heap, stmt) => accumulate(stmt, store, cache, heap) }
          accumulate(expr, store, cache, heap1)

        case Assign(lhs, rhs) =>
          debug.println(i"#!acc-assign {{{")
          debug.println(i"#lhs {${lhs.productPrefix}} {{{\n${lhs}\n}}}")
          debug.println(i"#rhs {${rhs.productPrefix}} {{{\n${rhs}\n}}}")
          debug.println("}}}")

          heap

        // TODO array mutations have their own magick symbol-less methods

        case Apply(sel @ Select(obj, _), args) if sel.symbol.isSetter && sel.symbol.is(Flags.Mutable) =>
          val sym = sel.symbol.underlyingSymbol
          debug.println(i"#!acc-apply-setter {{{")
          debug.println(i"#sel {${sel.productPrefix}} {{{\n${sel}\n}}}")
          debug.println(i"#sel.symbol.flags ${sel.symbol.flags.flagsString}")
          for (arg, i) <- args.iterator.zipWithIndex
          do debug.println(i"#args[$i] ${arg}")
          debug.println(i"#sym ${sym}")
          debug.println(i"#sel.symbol.accessedFieldOrGetter ${sel.symbol.accessedFieldOrGetter}")
          debug.println(i"#sel.symbol.name ${sel.symbol.name}")
          import dotty.tools.dotc.core.NameOps.TermNameDecorator
          val fieldName = sel.symbol.name.asTermName.getterName
          debug.println(i"#sel.symbol.name.asTermName.getterName ${fieldName}")
          debug.println(i"#fieldName.## ${fieldName.##}")
          val d = sel.symbol.owner.info.decl(fieldName)
          debug.println(i"#sel.symbol.owner.info.decl(fieldName) ${d}")
          debug.println(i"#d.getClass ${d.getClass}")
          debug.println(i"#d.symbol.flags.flagsString ${d.symbol.flags.flagsString}")
          debug.println(i"#d.symbol.## ${d.symbol.##}")
          debug.println(i"#d.## ${d.##}")
          debug.println(i"#d.info ${d.info}")
          debug.println(i"#d.info.isParameterless ${d.info.isParameterless}")
          val field = d.suchThat(!_.is(Flags.Method)).symbol
          val getter = d.suchThat(_.info.isParameterless).symbol
          debug.println(i"#field ${field}")
          debug.println(i"#getter ${getter}")
          debug.println("}}}")

          val arg :: Nil = args // expecting only one arg for a setter
          val abstractArg = _analyse(arg)

          (for {
            (ap, ls) <- _analyse(obj).iterator // TODO evaluating obj /can/ produce side-effects
            if ls.isEmpty // TODO only the strongs must be empty, find an error case
          } yield ap).foldLeft(heap) { (heap, ap) => heap.updatedWith(ap, d.symbol, abstractArg) {
            av => AV.merge(av.iterator ++ abstractArg)
          }}

        case Apply(meth, args) =>
          debug.println(i"#!acc-apply {{{")
          debug.println(i"#meth {${meth.productPrefix}} {{{\n${meth}\n}}}")
          debug.println(i"#meth.toString ${meth.toString}")
          debug.println(i"#meth.symbol ${meth.symbol}")
          for (arg, i) <- args.iterator.zipWithIndex
          do debug.println(i"#args[$i] ${arg}")
          debug.println("}}}")

          heap

        case _ => heap
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

        case tpd.Literal(_) => Map(AP.Constant -> LabelSet.empty)

        case tree @ Apply(sel @ Select(obj, _), Nil) if sel.symbol.isGetter && sel.symbol.is(Flags.Mutable) =>
          val sym = sel.symbol.underlyingSymbol
          debug.println(i"#sel.symbol.## ${sel.symbol.##}")
          debug.println(i"#sel.symbol.name.## ${sel.symbol.name.##}")
          debug.println(i"#!sym.flags.flagsString ${sym.flags.flagsString}")
          AV.merge {
            (for {
              (ap, ls) <- loop(obj).iterator
              if ls.isEmpty // TODO only the strongs must be empty, find a test case
            } yield {
              heap.getOrElse(ap, sym, {
                debug.println(i"#!!! Mutable symbol absent from heap! AP: ${ap.display} ; sym: ${sym}")
                AV.empty
              })
            }).flatten
          }

        case This(_) =>
          store.get(thisStoreKey) match {
            case None =>
              debug.println(i"#!!! @This tree, `this` absent from store!")
              Map.empty // TODO AnyValue?
            case Some(av) => av
          }

        case tree @ Select(This(_), name) =>
          // assuming that we have a member
          debug.println(i"#member-symbol-select {name=${name}#${name.##}}")

          store.get(thisStoreKey) match {
            case None =>
              debug.println(i"#!!! Tried to look up $name from the store, but `this` is absent")
              Map.empty // TODO AnyValue?
            case Some(av) =>
              av.filter { case (_, value) => value.weak.contains(name) }
          }

        case tree @ Ident(name) if tree.symbol.owner.isClass =>
          // assuming that we have a member
          debug.println("#member-symbol-ident")
          store.get(thisStoreKey) match {
            case None =>
              debug.println(i"!!! Tried to look up $name from the store, but `this` is absent")
              Map.empty // TODO AnyValue?
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
                  (key, labelSet) <- av
                } yield key -> (labelSet + param.name)
              }
          )

        /// method call
        case tree @ Apply(sel @ Select(objTree, ident), args) =>
          val abstractObj = loop(objTree, store)
          val iters =
            for
              (ap, emptyLabels) <- abstractObj.iterator
              if emptyLabels.isEmpty // TODO only the /strong/ set should be empty, find an error case for this
              // TODO if types overlap (why types need to overlap? does it make more sense to check if ident exists on ap?)
            yield
              (ap: @unchecked) match {
                case AP.Tree(cls @ Closure(env, meth, _)) =>
                  val methDef = meth.symbol.defTree.asInstanceOf[DefDef]
                  val closureBody = methDef.rhs

                  val (newStore: Store, abstractArgs: List[AV]) =
                    var res: Store = store

                    val abstractArgsBld = ListBuffer.empty[AV]
                    val params = methDef.vparamss.head.drop(env.length)
                    for
                      (vp, arg) <- params lazyZip args // TODO assuming that closures/constructors have exactly one parameter list
                    do
                      val abstractArg = loop(arg)
                      abstractArgsBld += abstractArg
                      res = res.updated(vp.symbol, abstractArg)

                    val envSyms = methDef.vparamss.head.take(env.length).map(_.symbol)
                    for
                      (ap, ls) <- abstractObj.iterator
                      sym <- envSyms.find(s => ls.weak.contains(s.name))
                    do
                      res = res.updated(sym, Map(ap -> ls))

                    (res, abstractArgsBld.result)

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
                      Map.empty
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
                      Map.empty // TODO: this should be /all/ possible values
                    case None =>
                      loop(methDef.rhs, newStore, cache.updated((ap, abstractArgs), "err"))
                  }

                case AP.Sym(sym) =>
                  // What does it /mean/ when we have a symbol in the map?
                  // It means we are dealing with a local variable.
                  // Therefore, we should first check if we can get a signature of the method being selected
                  // or otherwise we assume the worst
                  // should probably deal with the easy case first, where we're calling .apply on lambda we have signature of
                  val info = ctx.atPhase(ctx.postTyperPhase) {
                    sym.denot.info
                  }
                  if ident == nme.apply
                    && defn.isFunctionType(info)//.reporting(i"isFunctionType(info) = $result", debug)
                  then {
                    // TODO assuming that closures/constructors have exactly one parameter list
                    val AppliedType(_, params) = info // assuming that a "function type" is always an applied type
                    val iters =
                      for
                        (param, arg) <- params lazyZip args // discards the result type parameter
                        if !param.hasAnnotation(defn.LocalAnnot)
                      yield
                        loop(arg)
                    AV.merge(
                      // TODO a function *can* in some cases return itself, how to deal with that?
                      // Iterator(ap -> LabelSet.empty) ++
                        iters.iterator.flatten
                    )
                  } else {
                    // TODO retrieve signatures for arbitrary methods
                    AV.merge(
                      Iterator(ap -> LabelSet.empty) ++
                      args.iterator.map(loop(_)).flatten
                    )
                  }

                case AP.Constant =>
                  AV.merge(
                    args.iterator.map(loop(_)).flatten
                      ++ Iterator(AP.Constant -> LabelSet.empty)
                  )
              }

          AV.merge(iters.flatMap(_.iterator))

        case tree @ Apply(fun: Ident, args) if fun.symbol.exists =>
          val fsym = fun.symbol
          val symTree = fsym.defTree.asInstanceOf[DefDef] // StoreAnnotations phase only adds this annot to DefDefs
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

                  val (closureLocalParams: LocalParams, newStore: Store) =
                    var res1: LocalParams = SimpleIdentitySet.empty
                    var res2: Store = store
                    closureParams.iterator.zipWithIndex.foreach { case (p, idx) =>
                      if lpIndices.contains(idx) then res1 += p.symbol
                      res2 = res2.updated(p.symbol, Map(AP(p.symbol) -> LabelSet.empty))
                    }
                    for (p, t) <- closureEnvParams lazyZip env
                    do
                      debug.println(i"#env-param ${p.name} → {{{\n${t}\n}}}")
                      res2 = res2.updated(p.symbol, loop(t))
                    (res1, res2)

                  val finalExpr = finalExprOf(underlyingDefDef.rhs)

                  debug.println(i"#starting-local-analysis: ${clos.##}")

                  val analysed: AV = loop(finalExpr, newStore)

                  val escapees = analysed.keysIterator.filter(ap => closureLocalParams.contains(ap.maybeSym)).toSeq
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
          AV.empty
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
        res2 = res2.updated(vp.symbol, Map(AP(vp.symbol) -> LabelSet.empty))

      (res1, res2)
    }

    if (localParams.size != 0)
      debug.println(i"local params for ${tree.symbol} ==> ${localParams.toList}%, %")

    val finalExpr = finalExprOf(tree.rhs)

    // import given Flags.FlagOps
    if (localParams.size > 0) {
      debug.println(i"${tree.name}: detected local params")
      val heap = accumulate(tree.rhs, localStore.toMap, Map.empty, Heap.Empty)
      val analysed = analyse(finalExpr, localStore.toMap, Map.empty, heap)
      val resolved = AV.resolve(analysed, heap)
      val escapees = resolved.keysIterator.filter(ap => localParams.contains(ap.maybeSym)).toSeq
      if (escapees.size > 0) {
        ctx.error(i"(m) escaping locals: ${escapees}%, %", finalExpr.sourcePos)
      } else {
        debug.println("No locals escape")
      }
    }

    tree
  }

  private def finalExprOf(tree: Tree): Tree = {
    var finalExpr: Tree = tree
    def unblocked: Tree =
      finalExpr match {
        case Block(_, expr) => expr
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

  type AV = Map[AP, LabelSet]
  object AV {
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

    def empty: AV = Map.empty

    def merge(labels: Iterator[(AP, LabelSet)]): AV = {
      val resIter = for {
        (a, g) <- labels.toArray.groupBy(_._1).iterator
      } yield a -> (g.map(_._2).reduce(LabelSet.merge))
      resIter.toMap
    }

    def display(av: AV, indent: Int = 0)(implicit ctx: Context): String = {
      val res = new StringBuffer
      val spaces = " " * indent
      var nl_? = false
      def nl() =
        if nl_? then res append "\n"
        nl_? = true

      for (k, v) <- av
      do
        nl()
        res append spaces
        res append k.display
        res append s" ← ${v.display}"

      res.toString
    }
  }

  type Cache = Map[(AP, List[AV]), AV | "err"]

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
