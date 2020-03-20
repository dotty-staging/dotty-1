package dotty.tools.dotc
package sbt

import ast.{Trees, tpd}
import core._
import core.Decorators._
import Annotations._
import Contexts._
import Flags._
import Phases._
import Trees._
import Types._
import Symbols._
import NameOps._
import NameKinds.DefaultGetterName
import typer.Inliner
import transform.ValueClasses
import transform.SymUtils._
import dotty.tools.io.File
import java.io.PrintWriter

import xsbti.api.DefinitionType

import scala.collection.mutable
import scala.util.chaining._

/** This phase sends a representation of the API of classes to sbt via callbacks.
 *
 *  This is used by sbt for incremental recompilation.
 *
 *  See the documentation of `ExtractAPICollector`, `ExtractDependencies`,
 *  `ExtractDependenciesCollector` and
 *  http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html for more
 *  information on incremental recompilation.
 *
 *  The following flags affect this phase:
 *   -Yforce-sbt-phases
 *   -Ydump-sbt-inc
 *
 *  @see ExtractDependencies
 */
class ExtractAPICallback extends Phase {
  override def phaseName: String = "api-callback"

  override def isRunnable(implicit ctx: Context): Boolean = {
    def forceRun = ctx.settings.YforceSbtPhases.value
    super.isRunnable && (ctx.apiCallback != null || forceRun)
  }

  // Check no needed. Does not transform trees
  override def isCheckable: Boolean = false

  // SuperAccessors need to be part of the API (see the scripted test
  // `trait-super` for an example where this matters), this is only the case
  // after `PostTyper` (unlike `ExtractDependencies`, the simplication to trees
  // done by `PostTyper` do not affect this phase because it only cares about
  // definitions, and `PostTyper` does not change definitions).
  override def runsAfter: Set[String] = Set(transform.PostTyper.name)

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    val sourceFile = unit.source.file
    if (ctx.apiCallback != null) {
      ctx.apiCallback.startSource(sourceFile.jpath)
    }
    // if (ctx.sbtCallback != null) {
    //   ctx.sbtCallback.startSource(sourceFile.file)
    // }

    val apiTraverser = new APICallbackCollector
    val classes = apiTraverser.apiSource(unit.tpdTree)
    // val mainClasses = apiTraverser.mainClasses

    // if (ctx.settings.YdumpSbtInc.value) {
    //   // Append to existing file that should have been created by ExtractDependencies
    //   val pw = new PrintWriter(File(sourceFile.jpath).changeExtension("inc").toFile
    //     .bufferedWriter(append = true), true)
    //   try {
    //     classes.foreach(source => pw.println(DefaultShowAPI(source)))
    //   } finally pw.close()
    // }

    // if (ctx.sbtCallback != null) {
    //   classes.foreach(ctx.sbtCallback.api(sourceFile.file, _))
    //   mainClasses.foreach(ctx.sbtCallback.mainClass(sourceFile.file, _))
    // }
    if (ctx.apiCallback != null) {
      ctx.apiCallback.endSource()
    }
  }
}

/** Extracts full (including private members) API representation out of Symbols and Types.
 *
 *  The exact representation used for each type is not important: the only thing
 *  that matters is that a binary-incompatible or source-incompatible change to
 *  the API (for example, changing the signature of a method, or adding a parent
 *  to a class) should result in a change to the API representation so that sbt
 *  can recompile files that depend on this API.
 *
 *  Note that we only records types as they are defined and never "as seen from"
 *  some other prefix because `Types#asSeenFrom` is a complex operation and
 *  doing it for every inherited member would be slow, and because the number
 *  of prefixes can be enormous in some cases:
 *
 *    class Outer {
 *      type T <: S
 *      type S
 *      class A extends Outer { /*...*/ }
 *      class B extends Outer { /*...*/ }
 *      class C extends Outer { /*...*/ }
 *      class D extends Outer { /*...*/ }
 *      class E extends Outer { /*...*/ }
 *    }
 *
 *  `S` might be refined in an arbitrary way inside `A` for example, this
 *  affects the type of `T` as seen from `Outer#A`, so we could record that, but
 *  the class `A` also contains itself as a member, so `Outer#A#A#A#...` is a
 *  valid prefix for `T`. Even if we avoid loops, we still have a combinatorial
 *  explosion of possible prefixes, like `Outer#A#B#C#D#E`.
 *
 *  It is much simpler to record `T` once where it is defined, but that means
 *  that the API representation of `T` may not change even though `T` as seen
 *  from some prefix has changed. This is why in `ExtractDependencies` we need
 *  to traverse used types to not miss dependencies, see the documentation of
 *  `ExtractDependencies#usedTypeTraverser`.
 *
 *  TODO: sbt does not store the full representation that we compute, instead it
 *  hashes parts of it to reduce memory usage, then to see if something changed,
 *  it compares the hashes instead of comparing the representations. We should
 *  investigate whether we can just directly compute hashes in this phase
 *  without going through an intermediate representation, see
 *  http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html#Hashing+an+API+representation
 */
private class APICallbackCollector(implicit val ctx: Context) extends ThunkHolder {
  import tpd._
  import xsbti.api

  @inline final def apiCallback(op: APICallback => Unit)(implicit ctx: Context): Unit = {
    if (ctx.apiCallback != null) {
      op(ctx.apiCallback)
    }
  }

  private var cacheID: Long = -1L

  private def newId(): Long =
    cacheID += 1
    cacheID
  end newId

  /** This cache is necessary for correctness, see the comment about inherited
   *  members in `apiClassStructure`
   */
  private val classLikeCache = new mutable.HashMap[ClassSymbol, Long]
  /** This cache is optional, it avoids recomputing representations */
  private val typeCache = new mutable.HashMap[Type, Long]
  /** This cache is necessary to avoid unstable name hashing when `typeCache` is present,
   *  see the comment in the `RefinedType` case in `computeType`
   *  The cache key is (api of RefinedType#parent, api of RefinedType#refinedInfo).
    */
  private val refinedTypeCache = new mutable.HashMap[(Long, RefinedTypeHash), Long]

  private sealed abstract class RefinedTypeHash
  private final case class TypeAliasHash(name: String, alias: Long) extends RefinedTypeHash
  private final case class TypeBoundsHash(name: String, lo: Long, hi: Long) extends RefinedTypeHash

  // private val allNonLocalClassesInSrc = new mutable.HashSet[xsbti.api.ClassLike]
  private val _mainClasses = new mutable.HashSet[String]

  /** Some Dotty types do not have a corresponding type in xsbti.api.* that
   *  represents them. Until this is fixed we can workaround this by using
   *  special annotations that can never appear in the source code to
   *  represent these types.
   *
   *  @param tp      An approximation of the type we're trying to represent
   *  @param marker  A special annotation to differentiate our type
   */
  private def withMarker(tp: => Unit, marker: => Unit): Unit = apiCallback { cb =>
    // api.Annotated.of(tp, Array(marker))
    cb.startAnnotated()
    tp
    cb.startAnnotationSequence()
    marker
    cb.endAnnotationSequence()
    cb.endAnnotated()
  }


  private def marker(name: String): Unit = apiCallback { cb =>
    // api.Annotation.of(api.Constant.of(Constants.emptyType, name), Array())
    cb.startAnnotation()
    cb.startConstant(name)
    cb.emptyType()
    cb.endConstant()
    emptyAnnotationArgumentSequence()
    cb.endAnnotation()
  }

  private def orMarker = marker("Or")
  private def byNameMarker = marker("ByName")
  private def matchMarker = marker("Match")

  private def emptyAnnotationArgumentSequence(): Unit = apiCallback { cb =>
    cb.startAnnotationArgumentSequence()
    cb.endAnnotationArgumentSequence()
  }

  private def emptyAnnotationSequence(): Unit = apiCallback { cb =>
    cb.startAnnotationSequence()
    cb.endAnnotationSequence()
  }

  private def emptyClassDefinitionSequence(): Unit = apiCallback { cb =>
    cb.startClassDefinitionSequence()
    cb.endClassDefinitionSequence()
  }

  private def emptyTypeParameterSequence(): Unit = apiCallback { cb =>
    cb.startTypeParameterSequence()
    cb.endTypeParameterSequence()
  }

  private def typeSequence(op: => Unit): Unit = apiCallback { cb =>
    cb.startTypeSequence()
    op
    cb.endTypeSequence()
  }

  private def classDefinitionSequence(op: => Unit): Unit = apiCallback { cb =>
    cb.startClassDefinitionSequence()
    op
    cb.endClassDefinitionSequence()
  }

  private def typeParameterSequence(op: => Unit): Unit = apiCallback { cb =>
    cb.startTypeParameterSequence()
    op
    cb.endTypeParameterSequence()
  }

  /** Extract the API representation of a source file */
  def apiSource(tree: Tree): Unit = apiCallback { cb =>
    def apiClasses(tree: Tree): Unit = tree match {
      case PackageDef(_, stats) =>
        stats.foreach(apiClasses)
      case tree: TypeDef =>
        apiClass(tree.symbol.asClass)
      case _ =>
    }

    apiClasses(tree)
    cb.forceDelayedTasks()
  }

  /** Fusion of forceId with computeOrFetch
   */
  private def cacheCallback[T](key: T, map: mutable.Map[T, Long])(compute: T => Unit)(cb: APICallback): Unit = {
    if (map.contains(key)) {
      cb.sharedValue(map(key))
    } else {
      computeRegister(key)({ newIdFor(key, map) })(compute)(cb)
    }
  }

  private def computeOrFetch[T](id: Long, key: T, computed: Boolean)(compute: T => Unit)(cb: APICallback): Unit = {
    if (computed) {
      cb.sharedValue(id)
    } else {
      computeRegister(key)(id)(compute)(cb)
    }
  }

  private def newIdFor[T](key: T, map: mutable.Map[T, Long]): Long = {
    newId().tap(map.put(key, _))
  }

  private def forceId[T](key: T, map: mutable.Map[T, Long]): (Long, Boolean) = {
    if (map.contains(key)) {
      map(key) -> true
    } else {
      newIdFor(key, map) -> false
    }
  }

  private def computeRegister[T](key: T)(idGen: => Long)(compute: T => Unit)(cb: APICallback): Unit = {
    compute(key)
    cb.registerSharedWith(idGen)
  }

  def apiClass(sym: ClassSymbol): Unit = apiCallback(cacheCallback(sym, classLikeCache)(computeClass))

  def mainClasses: Set[String] = {
    apiCallback(_.forceDelayedTasks())
    _mainClasses.toSet
  }

  private def computeClass(sym: ClassSymbol): Unit /*api.ClassLikeDef*/ = apiCallback { cb =>
    import APICallback.{DefinitionType => dt}
    val defType =
      if (sym.is(Trait)) dt.TRAIT
      else if (sym.is(ModuleClass)) {
        if (sym.is(PackageClass)) dt.PACKAGE_MODULE
        else dt.MODULE
      } else dt.CLASS_DEF

    def childrenOfSealedClass(): Unit = typeSequence {
      sym.children.sorted(classFirstSort).foreach(c =>
        if (c.isClass)
          apiType(c.typeRef)
        else
          apiType(c.termRef)
      )
    }

    def tparams(): Unit = typeParameterSequence { sym.typeParams.foreach(apiTypeParameter) }

    def selfType(): Unit = evaluatedTask { apiType(sym.givenSelfType) }
    def structure(): Unit = evaluatedTask { apiClassStructure(sym) }

    def savedAnnotations(): Unit = {
      cb.startStringSequence()
      cb.endStringSequence()
    }

    val name = sym.fullName.stripModuleClassSuffix.toString
    val topLevel = sym.isTopLevelClass
      // We strip module class suffix. Zinc relies on a class and its companion having the same name

    // val cl = api.ClassLike.of(
    //   name, acc, modifiers, anns, defType, api.SafeLazy.strict(selfType), api.SafeLazy.strict(structure), Constants.emptyStringArray,
    //   childrenOfSealedClass, topLevel, tparams)
    cb.startClassLike(defType, name, topLevel)
    apiAccess(sym)
    apiModifiers(sym)
    apiAnnotations(sym)
    selfType()
    tparams()
    structure()
    childrenOfSealedClass()
    savedAnnotations()
    cb.endClassLike()

    cb.saveNonLocalClass() // allNonLocalClassesInSrc += cl

    if (sym.isStatic && !sym.is(Trait) && ctx.platform.hasMainMethod(sym)) {
      // If sym is an object, all main methods count, otherwise only @static ones count.
      // _mainClasses += name
      cb.registerMainClass(name)
    }

    // api.ClassLikeDef.of(name, acc, modifiers, anns, tparams, defType)
    cb.startClassLikeDef(defType, name)
    apiAccess(sym) // TODO reuse with a cache on client side?
    apiModifiers(sym) // TODO reuse with a cache on client side?
    apiAnnotations(sym) // TODO reuse with a cache on client side?
    tparams() // TODO reuse with a cache on client side?
    cb.endClassLikeDef
  }

  def delayTask(t: => Unit): Unit = apiCallback(_.delayTask(() => t))

  def evaluatedTask(t: => Unit): Unit = apiCallback { cb =>
    cb.startEvaluatedTask()
    t
    cb.endEvaluatedTask()
  }

  def apiClassStructure(csym: ClassSymbol): Unit /*api.Structure*/ = apiCallback { cb =>
    val cinfo = csym.classInfo

    val bases = {
      val ancestorTypes0 =
        try linearizedAncestorTypes(cinfo)
        catch {
          case ex: TypeError =>
            // See neg/i1750a for an example where a cyclic error can arise.
            // The root cause in this example is an illegal "override" of an inner trait
            ctx.error(ex, csym.sourcePos)
            defn.ObjectType :: Nil
        }
      if (ValueClasses.isDerivedValueClass(csym)) {
        val underlying = ValueClasses.valueClassUnbox(csym).info.finalResultType
        // The underlying type of a value class should be part of the name hash
        // of the value class (see the test `value-class-underlying`), this is accomplished
        // by adding the underlying type to the list of parent types.
        underlying :: ancestorTypes0
      } else
        ancestorTypes0
    }

    def apiBases(bases: List[Type]): Unit = evaluatedTask {
      cb.startTypeSequence()
      bases.foreach(apiType)
      cb.endTypeSequence()
    }

    def apiDecls(decls: List[Symbol]): Unit = evaluatedTask {
      cb.startClassDefinitionSequence()
      apiDefinitions(decls)
      cb.endClassDefinitionSequence()
    }

    def apiInherited(inherited: List[Symbol]) = delayTask {
      cb.startClassDefinitionSequence()
      apiDefinitions(inherited)
      cb.endClassDefinitionSequence()
    }

    cb.startStructure()

    apiBases(bases)

    // Synthetic methods that are always present do not affect the API
    // and can therefore be ignored.
    def alwaysPresent(s: Symbol) = csym.is(ModuleClass) && s.isConstructor
    val decls = cinfo.decls.filter(!alwaysPresent(_))

    apiDecls(decls)

    val declSet = decls.toSet
    // TODO: We shouldn't have to compute inherited members. Instead, `Structure`
    // should have a lazy `parentStructures` field.
    val inherited = cinfo.baseClasses
      .filter(bc => !bc.is(Scala2x))
      .flatMap(_.classInfo.decls.filter(s => !(s.is(Private) || declSet.contains(s))))

    apiInherited(inherited)

    cb.endStructure()

    // api.Structure.of(api.SafeLazy.strict(apiBases.toArray), api.SafeLazy.strict(apiDecls.toArray), apiInherited)
  }

  def linearizedAncestorTypes(info: ClassInfo): List[Type] = {
    val ref = info.appliedRef
    // Note that the ordering of classes in `baseClasses` is important.
    info.baseClasses.tail.map(ref.baseType)
  }

  // The hash generated by sbt for definitions is supposed to be symmetric so
  // we shouldn't have to sort them, but it actually isn't symmetric for
  // definitions which are classes, therefore we need to sort classes to
  // ensure a stable hash.
  // Modules and classes come first and are sorted by name, all other
  // definitions come later and are not sorted.
  private object classFirstSort extends Ordering[Symbol] {
    override def compare(a: Symbol, b: Symbol) = {
      val aIsClass = a.isClass
      val bIsClass = b.isClass
      if (aIsClass == bIsClass) {
        if (aIsClass) {
          if (a.is(Module) == b.is(Module))
            a.fullName.toString.compareTo(b.fullName.toString)
          else if (a.is(Module))
            -1
          else
            1
        } else
          0
      } else if (aIsClass)
      -1
    else
      1
    }
  }

  def apiDefinitions(defs: List[Symbol]): Unit /*List[api.ClassDefinition]*/ = {
    // defs.sorted(classFirstSort).map(apiDefinition)
    defs.sorted(classFirstSort).foreach(apiDefinition)
  }

  def apiDefinition(sym: Symbol): Unit /*api.ClassDefinition*/ = apiCallback { cb =>

    def sharedValOrVar(): Unit = {
      apiAccess(sym)
      apiModifiers(sym)
      apiAnnotations(sym)
      apiType(sym.info)
    }

    if (sym.isClass) {
      apiClass(sym.asClass)
    } else if (sym.isType) {
      apiTypeMember(sym.asType)
    } else if (sym.is(Mutable, butNot = Accessor)) {
      // api.Var.of(sym.name.toString, apiAccess(sym), apiModifiers(sym),
      //   apiAnnotations(sym).toArray, apiType(sym.info))
      cb.startVar(sym.name.toString)
      sharedValOrVar()
      cb.endVar()
    } else if (sym.isStableMember && !sym.isRealMethod) {
      // api.Val.of(sym.name.toString, apiAccess(sym), apiModifiers(sym),
      //   apiAnnotations(sym).toArray, apiType(sym.info))
      cb.startVal(sym.name.toString)
      sharedValOrVar()
      cb.endVal()
    } else {
      apiDef(sym.asTerm)
    }

  }

  def apiDef(sym: TermSymbol): Unit/*api.Def*/ = apiCallback { cb =>
    import APICallback.ParameterModifier

    def paramLists(t: Type): Unit = {
      def inner(t: Type, start: Int = 0): Unit = t match {
        case pt: TypeLambda =>
          assert(start == 0)
          inner(pt.resultType)
        case mt @ MethodTpe(pnames, ptypes, restpe) =>
          // TODO: We shouldn't have to work so hard to find the default parameters
          // of a method, Dotty should expose a convenience method for that, see #1143
          val defaults =
            if (sym.is(DefaultParameterized)) {
              val qual =
                if (sym.isClassConstructor)
                  sym.owner.companionModule // default getters for class constructors are found in the companion object
                else
                  sym.owner
              pnames.indices.map(i =>
                qual.info.member(DefaultGetterName(sym.name, start + i)).exists)
            } else
              pnames.indices.map(Function.const(false))

          // api.ParameterList.of(params.toArray, mt.isImplicitMethod) :: inner(restpe, params.length)
          cb.startParameterList(mt.isImplicitMethod)
          pnames.lazyZip(ptypes).lazyZip(defaults).foreach { (pname, ptype, isDefault) =>
            cb.startMethodParameter(pname.toString, isDefault, ParameterModifier.PLAIN)
            apiType(ptype)
            cb.endMethodParameter()
          }
          cb.endParameterList()

          inner(restpe, pnames.length)
        case _ =>
          ()
      }

      cb.startParameterListSequence()
      inner(t)
      cb.endParameterListSequence()
    }

    def typeParams(tpe: Type): Unit = typeParameterSequence {
      tpe match {
        case pt: TypeLambda =>
          pt.paramNames.lazyZip(pt.paramInfos).foreach((pname, pbounds) =>
            apiTypeParameter(pname.toString, 0, pbounds.lo, pbounds.hi))
        case _ =>
          ()
      }
    }

    cb.startDef(sym.name.toString)
    apiAccess(sym)
    apiModifiers(sym)
    apiAnnotations(sym)
    typeParams(sym.info)
    paramLists(sym.info)
    apiType(sym.info.finalResultType.widenExpr)
    cb.endDef()
    // api.Def.of(sym.name.toString, apiAccess(sym), apiModifiers(sym),
    //   apiAnnotations(sym).toArray, tparams.toArray, vparamss.toArray, apiType(retTp))
  }

  def apiTypeMember(sym: TypeSymbol): Unit /*api.TypeMember*/ = apiCallback { cb =>
    val name = sym.name.toString
    val tpe = sym.info
    def shared(): Unit = {
      apiAccess(sym)
      apiModifiers(sym)
      apiAnnotations(sym)
      emptyTypeParameterSequence()
    }
    if (sym.isAliasType) {
      cb.startTypeAlias(name)
      shared()
      apiType(tpe.bounds.hi)
      cb.endTypeAlias()
    }
    else {
      assert(sym.isAbstractType)
      cb.startTypeDeclaration(name)
      shared()
      apiType(tpe.bounds.lo)
      apiType(tpe.bounds.hi)
      cb.endTypeDeclaration()
    }
  }

  // Hack to represent dotty types which don't have an equivalent in xsbti
  /**
   *  @param apiTps operation that produces a sequence of types with an end marker
   */
  def combineApiTypes(apiTps: Type*): Unit /*api.Type*/ = apiCallback { cb =>
    // api.Structure.of(api.SafeLazy.strict(apiTps.toArray),
    //   api.SafeLazy.strict(Array()), api.SafeLazy.strict(Array()))
    cb.startStructure()
    evaluatedTask { typeSequence { apiTps.foreach(apiType) } }
    evaluatedTask { emptyClassDefinitionSequence() }
    evaluatedTask { emptyClassDefinitionSequence() }
    cb.endStructure()
  }

  def apiType(tp: Type): Unit /*: api.Type*/ = apiCallback(cacheCallback(tp, typeCache)(computeType))
  def forceTypeId(tp: Type): (Long, Boolean) = forceId(tp, typeCache)
  def forceApiType(tp: Type, id: Long, computed: Boolean)(cb: APICallback): Unit =
    computeOrFetch(id, tp, computed)(computeType)(cb)

  private def computeType(tp: Type): Unit/*: api.Type*/ = apiCallback { cb =>
    // TODO: Never dealias. We currently have to dealias because
    // sbt main class discovery relies on the signature of the main
    // method being fully dealiased. See https://github.com/sbt/zinc/issues/102
    val tp2 = if (!tp.isLambdaSub) tp.dealiasKeepAnnots else tp
    tp2 match {
      case NoPrefix | NoType =>
        // Constants.emptyType
        cb.emptyType()
      case tp: NamedType =>
        val sym = tp.symbol
        // A type can sometimes be represented by multiple different NamedTypes
        // (they will be `=:=` to each other, but not `==`), and the compiler
        // may choose to use any of these representation, there is no stability
        // guarantee. We avoid this instability by always normalizing the
        // prefix: if it's a package, if we didn't do this sbt might conclude
        // that some API changed when it didn't, leading to overcompilation
        // (recompiling more things than what is needed for incremental
        // compilation to be correct).
        val prefix = if (sym.maybeOwner.is(Package)) // { type T } here T does not have an owner
          sym.owner.thisType
        else
          tp.prefix

        // api.Projection.of(apiType(prefix), sym.name.toString)
        cb.startProjection(sym.name.toString)
        apiType(prefix)
        cb.endProjection()
      case AppliedType(tycon, args) =>
        def processArg(arg: Type): Unit /*api.Type*/ = arg match {
          case arg @ TypeBounds(lo, hi) => // Handle wildcard parameters
            if (lo.isDirectRef(defn.NothingClass) && hi.isDirectRef(defn.AnyClass))
              // Constants.emptyType
              cb.emptyType()
            else {
              val name = "_"
              // val ref = api.ParameterRef.of(name)
              // api.Existential.of(ref,
              //   Array(apiTypeParameter(name, 0, lo, hi)))
              cb.startExistential()
              typeParameterSequence { apiTypeParameter(name, 0, lo, hi) }
              cb.parameterRef(name)
              cb.endExistential()
            }
          case _ =>
            apiType(arg)
        }

        // val apiTycon = apiType(tycon)
        // val apiArgs = args.map(processArg)
        // api.Parameterized.of(apiTycon, apiArgs.toArray)
        cb.startParameterized()
        typeSequence { args.foreach(processArg) }
        apiType(tycon)
        cb.endParameterized()
      case tl: TypeLambda =>
        // val apiTparams = tl.typeParams.map(apiTypeParameter)
        // val apiRes = apiType(tl.resType)
        // api.Polymorphic.of(apiRes, apiTparams.toArray)
        cb.startPolymorphic()
        typeParameterSequence { tl.typeParams.foreach(apiTypeParameter) }
        apiType(tl.resType)
        cb.endPolymorphic()
      case rt: RefinedType =>
        val name = rt.refinedName.toString
        val (parent, parentComputed) = forceTypeId(rt.parent)

        def typeRefinement(name: String, tp: TypeBounds): (RefinedTypeHash, () => Unit) = tp match {
          case TypeAlias(alias) =>
            val (aliasId, computedAlias) = forceTypeId(alias)

            val computeDecl = () => {
              cb.startTypeAlias(name)
              cb.publicAPI()
              cb.modifiers(false, false, false, false, false, false, false, false)
              emptyAnnotationSequence()
              emptyTypeParameterSequence()
              forceApiType(alias, aliasId, computedAlias)(cb)
              cb.endTypeAlias()
            }

            TypeAliasHash(name, aliasId) -> computeDecl
            // api.TypeAlias.of(name,
            //   Constants.public, Constants.emptyModifiers, Array(), Array(), apiType(alias))
          case TypeBounds(lo, hi) =>
            val (loId, computedLo) = forceTypeId(lo)
            val (hiId, computedHi) = forceTypeId(hi)

            val computeDecl = () => {
              cb.startTypeDeclaration(name)
              cb.publicAPI()
              cb.modifiers(false, false, false, false, false, false, false, false)
              emptyAnnotationSequence()
              emptyTypeParameterSequence()
              forceApiType(lo, loId, computedLo)(cb)
              forceApiType(hi, hiId, computedHi)(cb)
              cb.endTypeDeclaration()
            }

            TypeBoundsHash(name, loId, hiId) -> computeDecl
            // api.TypeDeclaration.of(name,
            //   Constants.public, Constants.emptyModifiers, Array(), Array(), apiType(lo), apiType(hi))
        }
        val (hash, computeDecl) = rt.refinedInfo match {
          case rinfo: TypeBounds =>
            typeRefinement(name, rinfo)
          case _ =>
            ctx.debuglog(i"sbt-api: skipped structural refinement in $rt")
            (null, null)
        }

        // Aggressive caching for RefinedTypes: `typeCache` is enough as long as two
        // RefinedType are `==`, but this is only the case when their `refinedInfo`
        // are `==` and this is not always the case, consider:
        //
        //     val foo: { type Bla = a.b.T }
        //     val bar: { type Bla = a.b.T }
        //
        // The sbt API representations of `foo` and `bar` (let's call them `apiFoo`
        // and `apiBar`) will both be instances of `Structure`. If `typeCache` was
        // the only cache, then in some cases we would have `apiFoo eq apiBar` and
        // in other cases we would just have `apiFoo == apiBar` (this happens
        // because the dotty representation of `a.b.T` is unstable, see the comment
        // in the `NamedType` case above).
        //
        // The fact that we may or may not have `apiFoo eq apiBar` is more than
        // an optimisation issue: it will determine whether the sbt name hash for
        // `Bla` contains one or two entries (because sbt `NameHashing` will not
        // traverse both `apiFoo` and `apiBar` if they are `eq`), therefore the
        // name hash of `Bla` will be unstable, unless we make sure that
        // `apiFoo == apiBar` always imply `apiFoo eq apiBar`. This is what
        // `refinedTypeCache` is for.
        cacheCallback(parent -> hash, refinedTypeCache){ _ =>
          cb.startStructure()
          evaluatedTask {
            typeSequence {
              forceApiType(rt.parent, parent, parentComputed)(cb)
            }
          }
          evaluatedTask {
            if (hash == null) emptyClassDefinitionSequence()
            else classDefinitionSequence { computeDecl() }
          }
          evaluatedTask { emptyClassDefinitionSequence() }
          cb.endStructure()
        }(cb)
        // refinedTypeCache.getOrElseUpdate(parent -> hash, {
        //   val adecl: Array[api.ClassDefinition] = if (decl == null) Array() else Array(decl)
        //   api.Structure.of(api.SafeLazy.strict(Array(parent)), api.SafeLazy.strict(adecl), api.SafeLazy.strict(Array()))
        // })
      case tp: RecType =>
        apiType(tp.parent)
      case RecThis(recType) =>
        // `tp` must be present inside `recType`, so calling `apiType` on
        // `recType` would lead to an infinite recursion, we avoid this by
        //  computing the representation of `recType` lazily.
        apiLazy(recType)
      case tp: AndType =>
        // combineApiTypes(apiType(tp.tp1), apiType(tp.tp2))
        combineApiTypes(tp.tp1, tp.tp2)
      case tp: OrType =>
        // val s = combineApiTypes(apiType(tp.tp1), apiType(tp.tp2))
        // withMarker(s, orMarker)
        withMarker(combineApiTypes(tp.tp1, tp.tp2), orMarker)
      case ExprType(resultType) =>
        withMarker(apiType(resultType), byNameMarker)
      case MatchType(bound, scrut, cases) =>
        // val s = combineApiTypes(apiType(bound) :: apiType(scrut) :: cases.map(apiType): _*)
        // withMarker(s, matchMarker)
        withMarker(combineApiTypes(bound :: scrut :: cases: _*), matchMarker)
      case ConstantType(constant) =>
        // api.Constant.of(apiType(constant.tpe), constant.stringValue)
        cb.startConstant(constant.stringValue)
        apiType(constant.tpe)
        cb.endConstant()
      case AnnotatedType(tpe, annot) =>
        // api.Annotated.of(apiType(tpe), Array(apiAnnotation(annot)))
        cb.startAnnotated()
        apiType(tpe)
        cb.startAnnotationSequence()
        apiAnnotation(annot)
        cb.endAnnotationSequence()
        cb.endAnnotated()
      case tp: ThisType =>
        apiThis(tp.cls)
      case tp: ParamRef =>
        // TODO: Distinguishing parameters based on their names alone is not enough,
        // the binder is also needed (at least for type lambdas).

        // api.ParameterRef.of(tp.paramName.toString)
        cb.parameterRef(tp.paramName.toString)
      case tp: LazyRef =>
        apiType(tp.ref)
      case tp: TypeVar =>
        apiType(tp.underlying)
      case _ => {
        ctx.warning(i"sbt-api: Unhandled type ${tp.getClass} : $tp")
        // Constants.emptyType
        cb.emptyType()
      }
    }
  }

  def apiLazy(tp: => Type): /*api.Type*/ Unit = apiCallback { cb =>
    // TODO: The sbt api needs a convenient way to make a lazy type.
    // For now, we repurpose Structure for this.

    // val apiTp = lzy(Array(apiType(tp)))
    // api.Structure.of(apiTp, api.SafeLazy.strict(Array()), api.SafeLazy.strict(Array()))

    cb.startStructure()
    delayTask {
      typeSequence {
        apiType(tp)
      }
    }
    evaluatedTask { emptyClassDefinitionSequence() }
    evaluatedTask { emptyClassDefinitionSequence() }
    cb.endStructure()
  }

  def apiThis(sym: Symbol) /*: api.Singleton */ = apiCallback { cb =>
    // val pathComponents = sym.ownersIterator.takeWhile(!_.isEffectiveRoot)
    //   .map(s => api.Id.of(s.name.toString))
    // api.Singleton.of(api.Path.of(pathComponents.toArray.reverse ++ Array(Constants.thisPath)))
    cb.startSingleton()
    cb.startPath()
    cb.thisId()
    sym.ownersIterator
       .takeWhile(!_.isEffectiveRoot)
       .foreach(s => cb.id(s.name.toString))
    cb.endPath()
    cb.endSingleton()
  }

  def apiTypeParameter(tparam: ParamInfo): Unit =
    apiTypeParameter(tparam.paramName.toString, tparam.paramVarianceSign,
      tparam.paramInfo.bounds.lo, tparam.paramInfo.bounds.hi)

  def apiTypeParameter(name: String, variance: Int, lo: Type, hi: Type): Unit = apiCallback { cb =>
    // api.TypeParameter.of(name, Array(), Array(), apiVariance(variance), apiType(lo), apiType(hi))
    cb.startTypeParameter(name, apiVariance(variance))
    emptyAnnotationSequence()
    emptyTypeParameterSequence()
    apiType(lo)
    apiType(hi)
    cb.endTypeParameter()
  }

  def apiVariance(variance: Int): Int = {
    import APICallback.{Variance => v}
    if (variance < 0) v.CONTRAVARIANT
    else if (variance > 0) v.COVARIANT
    else v.INVARIANT
  }

  def apiAccess(sym: Symbol): Unit = apiCallback { cb =>
    // Symbols which are private[foo] do not have the flag Private set,
    // but their `privateWithin` exists, see `Parsers#ParserCommon#normalize`.
    if (!sym.isOneOf(Protected | Private) && !sym.privateWithin.exists)
      // Constants.public
      cb.publicAPI()
    else if (sym.isAllOf(PrivateLocal))
      // Constants.privateLocal
      cb.localAPI(false)
    else if (sym.isAllOf(ProtectedLocal))
      // Constants.protectedLocal
      cb.localAPI(true)
    else {
      val qualifier =
        if (sym.privateWithin eq NoSymbol)
          // Constants.unqualified
          null
        else
          // api.IdQualifier.of(sym.privateWithin.fullName.toString)
          sym.privateWithin.fullName.toString
      if (sym.is(Protected))
        // api.Protected.of(qualifier)
        cb.qualifiedAPI(true, qualifier)
      else
        // api.Private.of(qualifier)
        cb.qualifiedAPI(false, qualifier)
    }
  }

  def apiModifiers(sym: Symbol): Unit = apiCallback { cb =>
    val absOver = sym.is(AbsOverride)
    val abs = absOver || sym.isOneOf(Trait | Abstract | Deferred)
    val over = absOver || sym.is(Override)
    cb.modifiers(abs, over, sym.is(Final), sym.is(Sealed),
      sym.isOneOf(GivenOrImplicit), sym.is(Lazy), sym.is(Macro), sym.isSuperAccessor)
  }

  def apiAnnotations(s: Symbol): Unit /*List[api.Annotation]*/ = apiCallback { cb =>
    val inlineBody = Inliner.bodyToInline(s)
    if (!inlineBody.isEmpty) {
      // FIXME: If the body of an inlineable method changes, all the reverse
      // dependencies of this method need to be recompiled. sbt has no way
      // of tracking method bodies, so as a hack we include the pretty-printed
      // typed tree of the method as part of the signature we send to sbt.
      // To do this properly we would need a way to hash trees and types in
      // dotty itself.
      val printTypesCtx = ctx.fresh.setSetting(ctx.settings.XprintTypes, true)
      marker(inlineBody.show(printTypesCtx))
    }

    cb.startAnnotationSequence()

    // In the Scala2 ExtractAPI phase we only extract annotations that extend
    // StaticAnnotation, but in Dotty we currently pickle all annotations so we
    // extract everything (except inline body annotations which are handled
    // above).
    s.annotations.filter(_.symbol != defn.BodyAnnot) foreach { annot =>
      apiAnnotation(annot)
    }

    cb.endAnnotationSequence()
  }

  def apiAnnotation(annot: Annotation): Unit /*api.Annotation*/ = apiCallback { cb =>
    // FIXME: To faithfully extract an API we should extract the annotation tree,
    // sbt instead wants us to extract the annotation type and its arguments,
    // to do this properly we would need a way to hash trees and types in dotty itself,
    // instead we pretty-print the annotation tree.
    // However, we still need to extract the annotation type in the way sbt expect
    // because sbt uses this information to find tests to run (for example
    // junit tests are annotated @org.junit.Test).

    // api.Annotation.of(
    //   apiType(annot.tree.tpe), // Used by sbt to find tests to run
    //   Array(api.AnnotationArgument.of("FULLTREE", annot.tree.show)))
    cb.startAnnotation()
    apiType(annot.tree.tpe)
    cb.startAnnotationArgumentSequence()
    cb.annotationArgument("FULLTREE", annot.tree.show)
    cb.endAnnotationArgumentSequence()
    cb.endAnnotation()
  }
}
