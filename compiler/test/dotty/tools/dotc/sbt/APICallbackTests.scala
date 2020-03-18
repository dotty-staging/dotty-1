package dotty.tools.dotc.sbt

import java.nio.file.{Paths, Path}

import org.junit.Assert._
import org.junit.{Test => test, Before => before, After => after}
import org.junit.experimental.categories.Category

import scala.util.chaining._
import scala.collection.mutable

import dotty.BootstrappedOnlyTests
import dotty.tools.dotc.Main
import dotty.tools.dotc.sbt.APICallback
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, FreshContext}

@Category(Array(classOf[BootstrappedOnlyTests]))
class APICallbackTests:
  import APICallback.{DefinitionType => dt}
  import APICallbackTests._
  import APIEvent._

  val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.sbt.test"))

  @deprecated("out of date")
  def helloWorld: Unit = assertEquals(
    List(
      StartSource(path=rootSrc.resolve("HelloWorld.scala")),
        StartClassLike(dt.CLASS_DEF, name="example.HelloWorld", true),
          Public,
          Modifiers(isFinal=true),
          StartStructure,
            StartProjection("Object"),
              StartSingleton,
                StartPath,
                  ThisId,
                  Id("lang"),
                  Id("java"),
                EndPath,
              EndSingleton,
            EndProjection,
            StartProjection("Any"),
              StartSingleton,
                StartPath,
                  ThisId,
                  Id("scala"),
                EndPath,
              EndSingleton,
            EndProjection,
            EndParents,
            StartDef(name="<init>"),
              Public,
              Modifiers(),
              StartParamList(isImplicit=false),
              EndParamList,
              StartProjection(projected="HelloWorld"),
                StartSingleton,
                  StartPath,
                    ThisId,
                    Id("example"),
                  EndPath,
                EndSingleton,
              EndProjection,
            EndDef,
            StartVal(name="msg"),
              Public,
              Modifiers(isFinal=true),
              StartConstant(value="Hello, World!"),
                StartProjection(projected="String"),
                  StartSingleton,
                    StartPath,
                      ThisId,
                      Id("lang"),
                      Id("java"),
                    EndPath,
                  EndSingleton,
                EndProjection,
              EndConstant,
            EndVal,
            EndDecls,
          EndStructure,
        EndClassLike,
      EndSource
    ),
    compileOnly("HelloWorld.scala").events,
  )

  @test def helloWorldBuilder: Unit =
    val defs = compileOnlyBuild("HelloWorld.scala")
    println(defs)

  @test def boxBuilder: Unit =
    val defs = compileOnlyBuild("Box.scala")
    println(defs)

  @test def innerRecursiveBuilder: Unit =
    val defs = compileOnlyBuild("InnerRecursive.scala")
    println(defs)

  @test def classTParamsBuilder: Unit =
    val defs = compileOnlyBuild("ClassTParams.scala")
    println(defs)

  private def compileOnly(src: String): APIEventHolder =
    compile(rootSrc.resolve(src) :: Nil)

  private def compileOnlyBuild(src: String): List[xsbti.api.ClassLike] =
    compileBuild(rootSrc.resolve(src) :: Nil)

  private def compile(srcs: List[Path]): APIEventHolder =
    val (eventHolder, rootCtx) = setupEventHolder
    val exit = Main.process(args(srcs), rootCtx)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    eventHolder

  private def compileBuild(srcs: List[Path]): List[xsbti.api.ClassLike] =
    val (builder, rootCtx) = setupBuilder
    val exit = Main.process(args(srcs), rootCtx)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    builder.classLikes

end APICallbackTests

object APICallbackTests:
  import APIEvent._

  enum APIEvent:
    case StartSource(path: Path)
    case EndSource
    case StartClassLike(definitionType: Int, name: String, topLevel: Boolean)
    case StartClassLikeDef(definitionType: Int, name: String)
    case EndClassLike
    case StartStructure
    case EndParents
    case EndDecls
    case EndStructure
    case StartVal(name: String)
    case EndVal
    case StartDef(name: String)
    case EndDef
    case StartTypeParameter(name: String, variance: Int)
    case StartParamList(isImplicit: Boolean)
    case EndParamList
    case StartConstant(value: String)
    case EndConstant
    case StartProjection(projected: String)
    case EndProjection
    case StartSingleton
    case EndSingleton
    case StartPath
    case EndPath
    case Id(name: String)
    case ThisId
    case Public
    case Modifiers(
      isAbstract: Boolean = false,
      isOverride: Boolean = false,
      isFinal: Boolean = false,
      isSealed: Boolean = false,
      isImplicit: Boolean = false,
      isLazy: Boolean = false,
      isMacro: Boolean = false,
      isSuperAccessor: Boolean = false
    )

  trait EventConsumer:
    protected def accept(event: APIEvent): Unit

  trait APIEventHolder extends EventConsumer:
    private val _events = mutable.ArrayBuffer.empty[APIEvent]
    protected def accept(event: APIEvent): Unit = _events += event
    def events: List[APIEvent] = _events.toList
  end APIEventHolder

  trait TestAPICallback extends APICallback:
    self: EventConsumer =>

    override def startSource(src: Path): Unit = accept(StartSource(src))
    override def endSource(): Unit = accept(EndSource)

    override def startClassLike(dt: Int, name: String, topLevel: Boolean): Unit = accept(StartClassLike(dt, name, topLevel))
    override def endClassLike(): Unit = accept(EndClassLike)

    override def startStructure(): Unit = accept(StartStructure)
    override def endStructure(): Unit = accept(EndStructure)

    override def startVal(name: String): Unit = accept(StartVal(name))
    override def endVal(): Unit = accept(EndVal)

    override def startDef(name: String): Unit = accept(StartDef(name))
    override def endDef(): Unit = accept(EndDef)

    override def startParameterList(isImplicit: Boolean): Unit = accept(StartParamList(isImplicit))
    override def endParameterList(): Unit = accept(EndParamList)

    override def startProjection(selected: String): Unit = accept(StartProjection(selected))
    override def endProjection(): Unit = accept(EndProjection)

    override def startConstant(value: String): Unit = accept(StartConstant(value))
    override def endConstant(): Unit = accept(EndConstant)

    override def startSingleton(): Unit = accept(StartSingleton)
    override def endSingleton(): Unit = accept(EndSingleton)

    override def startPath(): Unit = accept(StartPath)
    override def endPath(): Unit = accept(EndPath)

    override def id(name: String): Unit = accept(Id(name))
    override def thisId(): Unit = accept(ThisId)
    override def publicAPI(): Unit = accept(Public)
    override def modifiers(
      isAbstract: Boolean,
      isOverride: Boolean,
      isFinal: Boolean,
      isSealed: Boolean,
      isImplicit: Boolean,
      isLazy: Boolean,
      isMacro: Boolean,
      isSuperAccessor: Boolean
    ): Unit = accept(Modifiers(isAbstract, isOverride, isFinal, isSealed, isImplicit, isLazy, isMacro, isSuperAccessor))

  end TestAPICallback

  trait BuilderAPICallback extends APICallback with ThunkHolder:

    import xsbti.api
    import BuilderAPICallback._

    private val cache = mutable.LongMap.empty[Any]
    private val allNonLocalClassesInSrc = new mutable.HashSet[api.ClassLike]
    private val _mainClasses = new mutable.HashSet[String]

    private var stack = List.empty[Any] // replace with mutable.ArrayDeque.empty[Any]
    private def modify[A,B](f: A => B) = stack = f(stack.head.asInstanceOf[A]) :: stack.tail
    private def accept(any: Any) = stack ::= any
    private def peelFront[T: reflect.ClassTag](qual: Any => Boolean): (Array[T], List[Any]) =
      (stack.takeWhile(qual).asInstanceOf[List[T]].reverse.toArray, stack.dropWhile(qual))
    private def collectSequence[T: reflect.ClassTag](qual: Any => Boolean): Unit =
      stack = stack.takeWhile(qual).asInstanceOf[List[T]].reverse.toArray :: stack.dropWhile(qual)

    def classLikes: List[api.ClassLike] = allNonLocalClassesInSrc.toList

    private def apiDefinitionType(dt: Int): api.DefinitionType = dt match {
      case APICallback.DefinitionType.CLASS_DEF      => api.DefinitionType.ClassDef
      case APICallback.DefinitionType.TRAIT          => api.DefinitionType.Trait
      case APICallback.DefinitionType.MODULE         => api.DefinitionType.Module
      case APICallback.DefinitionType.PACKAGE_MODULE => api.DefinitionType.PackageModule
    }

    private def apiVariance(dt: Int): api.Variance = dt match {
      case APICallback.Variance.CONTRAVARIANT => api.Variance.Contravariant
      case APICallback.Variance.COVARIANT     => api.Variance.Covariant
      case APICallback.Variance.INVARIANT     => api.Variance.Invariant
    }

    override def startClassLike(dt: Int, name: String, topLevel: Boolean): Unit = accept(StartClassLike(dt, name, topLevel))
    override def endClassLike(): Unit =
      val (savedAnnots: Array[String]) ::
          (children: Array[api.Type]) ::
          (structure: api.Lazy[Any]) ::
          (tparams: Array[api.TypeParameter]) ::
          (selfTpe: api.Lazy[Any]) ::
          (annots: Array[api.Annotation]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          StartClassLike(dt, name, topLevel) :: stack1 = stack
      stack = api.ClassLike.create(
        name, access, mods, annots, apiDefinitionType(dt), selfTpe.asInstanceOf,
        structure.asInstanceOf, savedAnnots, children, topLevel, tparams) :: stack1

    override def startClassLikeDef(dt: Int, name: String): Unit = accept(StartClassLikeDef(dt, name))
    override def endClassLikeDef(): Unit =
      val (tparams: Array[api.TypeParameter]) ::
          (annots: Array[api.Annotation]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          StartClassLikeDef(dt, name) :: stack1 = stack
      stack = api.ClassLikeDef.create(
        name, access, mods, annots, tparams, apiDefinitionType(dt)) :: stack1

    override def registerSharedWith(id: Long): Unit = cache.put(id, stack.head)
    override def sharedValue(id: Long): Unit = accept(cache(id))

    override def saveNonLocalClass(): Unit =
      allNonLocalClassesInSrc += stack.head.asInstanceOf[api.ClassLike]
      stack = stack.tail

    override def registerMainClass(name: String): Unit = _mainClasses += name

    override def startStructure(): Unit = ()
    override def endStructure(): Unit =
      val (inherited: api.Lazy[Any]) ::
          (decls: api.Lazy[Any]) ::
          (parents: api.Lazy[Any]) :: stack1 = stack
      stack = api.Structure.create(parents.asInstanceOf, decls.asInstanceOf, inherited.asInstanceOf) :: stack1

    override def endTypeSequence(): Unit =
      collectSequence[api.Type](_.isInstanceOf[api.Type])

    override def endClassDefinitionSequence(): Unit =
      collectSequence[api.ClassDefinition](_.isInstanceOf[api.ClassDefinition])

    override def endTypeParameterSequence(): Unit =
      collectSequence[api.TypeParameter](_.isInstanceOf[api.TypeParameter])

    override def endAnnotationSequence(): Unit =
      collectSequence[api.Annotation](_.isInstanceOf[api.Annotation])

    override def endAnnotationArgumentSequence(): Unit =
      collectSequence[api.AnnotationArgument](_.isInstanceOf[api.AnnotationArgument])

    override def endParameterListSequence(): Unit =
      collectSequence[api.ParameterList](_.isInstanceOf[api.ParameterList])

    override def endStringSequence(): Unit =
      collectSequence[String](_.isInstanceOf[String])

    override def startTypeParameter(name: String, variance: Int): Unit = accept(StartTypeParameter(name, variance))
    override def endTypeParameter(): Unit =
      val (hi: api.Type) ::
          (lo: api.Type) ::
          (tparams: Array[api.TypeParameter]) ::
          (annots: Array[api.Annotation]) ::
          StartTypeParameter(name, variance)  :: stack1 = stack
      stack = api.TypeParameter.create(name, annots, tparams, apiVariance(variance), lo, hi) :: stack1

    override def startEvaluatedTask(): Unit = ()
    override def endEvaluatedTask(): Unit = modify(api.SafeLazy.strict)

    override def delayTask(task: Runnable): Unit =
      val embedded = api.SafeLazy { () =>
        task.run()
        val result = stack.head
        stack = stack.tail
        result
      }
      accept(lzy(embedded))

    override def forceDelayedTasks(): Unit = forceThunks()

    override def startVal(name: String): Unit = accept(name)
    override def endVal(): Unit = endValOrVar(api.Val.create)

    override def startVar(name: String): Unit = accept(name)
    override def endVar(): Unit = endValOrVar(api.Var.create)

    private def endValOrVar[T](f: (String, api.Access, api.Modifiers, Array[api.Annotation], api.Type) => T): Unit =
      val (retTpe: api.Type) ::
          (annots: Array[api.Annotation]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          (name: String) :: stack1 = stack
      stack = f(name, access, mods, annots, retTpe) :: stack1

    override def startDef(name: String): Unit = accept(name)
    override def endDef(): Unit =
      val (retTpe: api.Type) ::
          (paramss: Array[api.ParameterList]) ::
          (tparams: Array[api.TypeParameter]) ::
          (annots: Array[api.Annotation]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          (name: String) :: stack1 = stack
      stack = api.Def.create(name, access, mods, annots, tparams, paramss, retTpe) :: stack1

    override def startParameterList(isImplicit: Boolean): Unit = accept(isImplicit)
    override def endParameterList(): Unit = modify((isImplicit: Boolean) => api.ParameterList.create(Array.empty, isImplicit))

    override def startTypeDeclaration(name: String): Unit = accept(name)
    override def endTypeDeclaration(): Unit =
      val (hi: api.Type) ::
          (lo: api.Type) ::
          (tparams: Array[api.TypeParameter]) ::
          (annots: Array[api.Annotation]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          (name: String) :: stack1 = stack
      stack = api.TypeDeclaration.create(name, access, mods, annots, tparams, lo, hi) :: stack1

    override def startTypeAlias(name: String): Unit = accept(name)
    override def endTypeAlias(): Unit =
      val (alias: api.Type) ::
          (tparams: Array[api.TypeParameter]) ::
          (annots: Array[api.Annotation]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          (name: String) :: stack1 = stack
      stack = api.TypeAlias.create(name, access, mods, annots, tparams, alias) :: stack1

    override def startAnnotation(): Unit = {}
    override def endAnnotation(): Unit =
      val (args: Array[api.AnnotationArgument]) ::
          (tpe: api.Type) :: stack1 = stack
      stack = api.Annotation.create(tpe, args) :: stack1

    override def startProjection(selected: String): Unit = accept(selected)
    override def endProjection(): Unit =
      val (prefix: api.Type) :: (selected: String) :: stack1 = stack
      stack = api.Projection.create(prefix, selected) :: stack1

    override def startConstant(value: String): Unit = accept(value)
    override def endConstant(): Unit =
      val (tpe: api.Type) :: (value: String) :: stack1 = stack
      stack = api.Constant.create(tpe, value) :: stack1

    override def startSingleton(): Unit = ()
    override def endSingleton(): Unit = modify((p: api.Path) => api.Singleton.create(p))

    override def startPath(): Unit = ()
    override def endPath(): Unit =
      val (components, stack1) = peelFront[api.PathComponent](_.isInstanceOf[api.PathComponent])
      stack = api.Path.create(components) :: stack1

    override def annotationArgument(name: String, value: String) = accept(api.AnnotationArgument.create(name, value))
    override def id(name: String): Unit = accept(api.Id.create(name))
    override def thisId(): Unit = accept(Constants.thisPath)

    override def publicAPI(): Unit = accept(Constants.public)
    override def localAPI(isProtected: Boolean): Unit = accept(if (isProtected) Constants.protectedLocal else Constants.privateLocal)
    override def qualifiedAPI(isProtected: Boolean, optionalQualifier: String | Null): Unit =
      val qualifier = if (optionalQualifier != null) api.IdQualifier.of(optionalQualifier) else Constants.unqualified
      accept(if (isProtected) api.Protected.of(qualifier) else api.Private.of(qualifier))

    override def emptyType(): Unit = accept(Constants.emptyType)
    override def modifiers(
      isAbstract: Boolean,
      isOverride: Boolean,
      isFinal: Boolean,
      isSealed: Boolean,
      isImplicit: Boolean,
      isLazy: Boolean,
      isMacro: Boolean,
      isSuperAccessor: Boolean
    ): Unit = accept(api.Modifiers(isAbstract, isOverride, isFinal, isSealed, isImplicit, isLazy, isMacro, isSuperAccessor))

  end BuilderAPICallback

  object BuilderAPICallback:

    import xsbti.api

    private object Constants:
      val emptyStringArray = Array[String]()
      val local            = api.ThisQualifier.create()
      val public           = api.Public.create()
      val privateLocal     = api.Private.create(local)
      val protectedLocal   = api.Protected.create(local)
      val unqualified      = api.Unqualified.create()
      val thisPath         = api.This.create()
      val emptyType        = api.EmptyType.create()
      val emptyModifiers   =
        new api.Modifiers(false, false, false, false, false,false, false, false)

  end BuilderAPICallback

  private val commonArgs = Array(
    "-Ystop-after:api-callback",
    "-usejavacp"
  )

  def args(srcs: List[Path]) = commonArgs ++ srcs.map(_.toString)

  def setupEventHolder: (APIEventHolder, FreshContext) =
    val cb = new TestAPICallback with APIEventHolder
    cb -> ContextBase().initialCtx.fresh.setAPICallback(cb)

  def setupBuilder: (BuilderAPICallback, FreshContext) =
    val cb = new BuilderAPICallback {}
    cb -> ContextBase().initialCtx.fresh.setAPICallback(cb)

end APICallbackTests
