package dotty.tools
package dotc
package transform
package eff

import core._
import Decorators._
import Symbols._
import Types._

import ast.tpd._
import core.Contexts
import core.Contexts.Context
import config.Printers.debug

import printing.{Showable, Printer}
import printing.Texts.Text

import transform.MegaPhase.MiniPhase

import StdNames.nme

import reporting.trace

import util.SimpleIdentitySet

class EscapeAnalysis extends MiniPhase {
  import EscapeAnalysis.{given _, _}

  def showResult(it: Any)(implicit ctx: Context): String =
    it match
      case r: (AV @unchecked) => s"{{{\n${AV.display(r)}\n}}}"
      case _ => s"{{{\n${ it }\n}}}"

  def analyse(
    tree: Tree,
    store: Store
    // need cache right around here
  )(implicit ctx: Context): AV =
    def loop(
      tree: Tree,
      _store: Store = store
    )(implicit ctx: Context) = analyse(tree, _store)

    def peek[S <: Showable](str: String, peeked: S): S =
      debug.println(i"$str: $peeked # ${peeked.toString}")
      peeked

    def unclutter(tree: Tree): Tree = tree match {
      case Typed(expr, _) => unclutter(expr)
      case Block(_, expr) => unclutter(expr)
      case TypeApply(expr, _) => unclutter(expr)
      case Apply(fun, expr :: Nil)
          // TODO clean this up - these symbols shouldn't be looked up every time we enter here
          if tree.symbol == ctx.requiredModule("scala.Long").requiredMethod("unbox")
          || tree.symbol == defn.IntClass.companionModule.requiredMethod("int2long")
          => expr
      case Select(expr, ident) if ident == nme.asInstanceOf_ => expr
      case expr => expr
    }

    val uncluttered = unclutter(tree)

    trace(i"analyse(${tersely(uncluttered)}; ${store.keys.map(_.name).toList}%, %)", debug, showResult) {
      debug.println("{{{")
      debug.println(i"#uncluttered = {{{\n${uncluttered}\n}}}")
      debug.println(s"#store = {{{\n${Store.display(store, 0)}\n}}}")
      debug.println(i"#tree = {{{\n${tree}\n}}}")
      debug.println("}}}")

      // case tree @ Apply(fun: New, args) if fun.symbol.isConstructor && !fun.symbol.defTree.isEmpty =>

      uncluttered match {
        // case tree @ New(_) =>
        //   // NOTE `New` tree signifies a `new` node w/o any parameters
        //   Map(AllocPoint(tree) -> LabelSet.empty)

        // // member selection
        // case tree @ Apply(fun @ HasSym(fsym), args) if fsym.isGetter =>
        //   debug.println(i"symbol is a getter")
        //   val Select(qual, name) = fun
        //   for {
        //     (key, labelSet) <- loop(qual)
        //     if (labelSet.weak.contains(name))
        //   } yield key -> labelSet.copy( weak = labelSet.weak - name )

        case tree @ Apply(fun @ Select(new_, ident), args) if ident == nme.CONSTRUCTOR =>
          val fsym = fun.symbol
          // TODO we need to see primary constructor to know the local variables
          // NOTE 
          assert(fsym.isPrimaryConstructor)
          val constructorDef = fsym.defTree.asInstanceOf[DefDef]
          val constructorVparams :: Nil = constructorDef.vparamss
          assert(args.hasSameLengthAs(constructorVparams))
          AV.merge(
            Iterator(AllocPoint(new_) -> LabelSet.empty) ++ {
              for {
                (param, arg) <- constructorVparams.iterator zip args.iterator
                (key, labelSet) <- loop(arg).iterator
              } yield key -> LabelSet(
                weak = labelSet.weak + param.name,
                strong = labelSet.strong + param.name
              )
            }
          )

        case tree @ Apply(sel @ Select(objTree, ident), args) =>
          val abstractObj = analyse(objTree, store)
          val iters =
            for
              (ap, emptyLabels) <- abstractObj.iterator
              if emptyLabels.isEmpty
              // TODO if types overlap (why types need to overlap? does it make more sense to check if ident exists on ap?)
            yield
              (ap.value: @unchecked) match {
                case Closure(env, meth, _) =>
                  val methDef = meth.symbol.defTree.asInstanceOf[DefDef]
                  val closureBody = methDef.rhs

                  val newStore: Store =
                    var res: Store = store

                    val params = methDef.vparamss.head.drop(env.length)
                    for
                      (vp, arg) <- params lazyZip args // TODO assuming that closures have exactly one parameter list
                    do
                      res = res.updated(vp.symbol, loop(arg))

                    val envSyms = methDef.vparamss.head.take(env.length).map(_.symbol)
                    for
                      (ap, ls) <- abstractObj.iterator
                      sym <- envSyms.find(s => ls.weak.contains(s.name))
                    do
                      res = res.updated(sym, Map(ap -> ls))
                    res

                  loop(closureBody, newStore)

                case New(_) =>
                  val methDefTree = sel.symbol.defTree
                  debug.println(i"#method-def-tree {${ident.toString}} {{{\n${methDefTree}\n}}}")

                  // TODO retrieve method body
                  // TODO retrieve method signature if body is unavailable
                  AV.merge(
                    abstractObj.iterator // TODO improve precision based on knowing that we call a method on `ap`
                      ++ args.iterator.map(loop(_)).flatten
                  )

                case sym: Symbol =>
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
                    // TODO assuming that closures have exactly one parameter list
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
                  // TODO assuming that closures have exactly one parameter list
                  // TODO inefficient length call
                  val closureParams = underlyingDefDef.vparamss.head.drop(env.length)
                  val closureEnvParams = underlyingDefDef.vparamss.head.take(env.length)

                  val (closureLocalParams: LocalParams, newStore: Store) =
                    var res1: LocalParams = SimpleIdentitySet.empty
                    var res2: Store = store
                    closureParams.iterator.zipWithIndex.foreach { case (p, idx) =>
                      if lpIndices.contains(idx) then res1 += p.symbol
                      res2 = res2.updated(p.symbol, Map(AllocPoint(p.symbol) -> LabelSet.empty))
                    }
                    for (p, t) <- closureEnvParams lazyZip env
                    do
                      debug.println(i"#env-param ${p.name} → {{{\n${t}\n}}}")
                      res2 = res2.updated(p.symbol, loop(t))
                    (res1, res2)

                  val finalExpr = finalExprOf(underlyingDefDef.rhs)

                  debug.println(i"#starting-local-analysis: ${clos.##}")

                  val analysed: AV = analyse(finalExpr, newStore)

                  val escapees = analysed.keysIterator.filter(ap => closureLocalParams.contains(ap.value)).toSeq
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
              (vp, arg) <- symTree.vparamss.head lazyZip args // TODO assuming that the closure has exactly one param list
            do
              res = res.updated(vp.symbol, loop(arg))
            res

          loop(symTree.rhs, newStore)

        case tree @ Ident(_) =>
          store.getOrElse(tree.symbol,
            tree.symbol.defTree match {
              case EmptyTree => sys.error(i"missing def tree for {${tree.symbol}} shouldn't happen, missing -Yretain-trees")
              case dt: ValDef =>
                debug.println(i"going into def tree of $tree")
                loop(dt.rhs)
            }
          )

        case tree @ Closure(env, meth, _) =>
          val closureDefTree = meth.symbol.defTree.asInstanceOf[DefDef]

          debug.println(i"!!! closure: ${meth.symbol} {{{")
          debug.println(i"env = [${env.length}] $env%, %")
          debug.println("}}}")

          // val closureDefinition @ (_: DefDef) = meth.symbol.defTree
          // loop(finalExprOf(closureDefinition.rhs))

          // TODO !!! this map should also contain variables captured by the closure

          /**
            * Represents a user-inaccessible name and marks values that were
            * captured and can be returned from an arbitrary method.
            */
          val capturedValueMarker = nme.EMPTY
          AV.merge(
            Iterator(AllocPoint(tree) -> LabelSet.empty)
              ++ (env lazyZip closureDefTree.vparamss.head).iterator.map { case (t, vt) =>
                loop(t).iterator.map {
                  case (ap, lbls) => ap -> LabelSet(
                    lbls.weak + capturedValueMarker + vt.symbol.name,
                    lbls.strong + capturedValueMarker + vt.symbol.name)
                }
              }.flatten
          )

        case tree =>
          debug.println(i"!!! UNRECOGNIZED TREE: empty abstract value for ${tree.getClass.getSimpleName}:{{{\n${ tree }\n${ tree.toString }\n}}}")
          AV.empty
      }
    }

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    val Apply(fun @ HasSym(fsym), args) = tree
    if fsym.exists && fsym.hasAnnotation(ctx.definitions.LocalParamsAnnot) then
      analyse(tree, store = Map.empty)
    tree
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context): Tree = {
    // debug.println(i"{{{\n# template =\n$tree}}}")
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {

    val (localParams: LocalParams, localStore: Store) = {
      var res1: LocalParams = SimpleIdentitySet.empty
      var res2: Store = Map.empty
      for
        vparams <- tree.vparamss
        vp <- vparams
      do
        if vp.symbol.hasAnnotation(ctx.definitions.LocalAnnot) then res1 += vp.symbol
        res2 = res2.updated(vp.symbol, Map(AllocPoint(vp.symbol) -> LabelSet.empty))

      (res1, res2)
    }

    if (localParams.size != 0)
      debug.println(i"local params for ${tree.symbol} ==> ${localParams.toList}%, %")

    val finalExpr = finalExprOf(tree.rhs)

    // import given Flags.FlagOps
    if (localParams.size > 0) {
      debug.println(i"${tree.name}: detected local params")
      val analysed = analyse(finalExpr, localStore.toMap)
      val escapees = analysed.keysIterator.filter(ap => localParams.contains(ap.value)).toSeq
      if (escapees.size > 0) {
        ctx.error(i"(m) escaping locals: ${escapees}%, %", finalExpr.sourcePos)
      } else {
        debug.println("No locals escape")
      }
    }

    tree
  }

  def finalExprOf(tree: Tree): Tree = {
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

  val phaseName = EscapeAnalysis.name
}

object EscapeAnalysis {

  val name = "EscapeAnalysis"

  type Label = Names.Name
  case class LabelSet(
    weak: Set[Label],
    strong: Set[Label]
  ) {
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

  // an AllocPoint is a Symbol iff it is a local param
  case class AllocPoint(value: Symbol | Tree) extends Showable {
    override def toText(printer: Printer): Text = {
      import printing._
      import Texts._

      val txt = value match {
        case v: Symbol => printer.toText(v)
        case v: Tree => printer.toText(v)
      }

      Str("AllocPoint(") ~ txt ~ Str(")")
    }

    // NOTE hashCode should work out OK, since all union members have it implemented
  }

  type AV = Map[AllocPoint, LabelSet]
  object AV {
    def empty: AV = Map.empty

    def merge(labels: Iterator[(AllocPoint, LabelSet)]): AV = {
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
      do k.value match
      case s: Symbol =>
        nl()
        res append s"${spaces}${s.name} ← ${v.display}"
      case t: Tree =>
        nl()
        res append s"${spaces}${tersely(t)}#${t.##} ← ${v.display}"
      res.toString
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
