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

  @test def innerRecursiveBuilder: Unit =
    val defs = compileOnlyBuild("InnerRecursive.scala")
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
    // override def endParents(): Unit = accept(EndParents)
    // override def endDecls(): Unit = accept(EndDecls)
    override def endStructure(): Unit = accept(EndStructure)

    override def startVal(name: String): Unit = accept(StartVal(name))
    override def endVal(): Unit = accept(EndVal)

    override def startDef(name: String): Unit = accept(StartDef(name))
    override def endDef(): Unit = accept(EndDef)

    override def startParamList(isImplicit: Boolean): Unit = accept(StartParamList(isImplicit))
    override def endParamList(): Unit = accept(EndParamList)

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

    private var stack = List.empty[Any] // replace with mutable.ArrayDeque.empty[Any]
    private def modify[A,B](f: A => B) = stack = f(stack.head.asInstanceOf[A]) :: stack.tail
    private def accept(any: Any) = stack ::= any
    private def peelFront[T: reflect.ClassTag](qual: Any => Boolean): (Array[T], List[Any]) =
      (stack.takeWhile(qual).asInstanceOf[List[T]].reverse.toArray, stack.dropWhile(qual))

    def classLikes: List[api.ClassLike] = allNonLocalClassesInSrc.toList

    private def definitionType(dt: Int): api.DefinitionType = dt match {
      case APICallback.DefinitionType.CLASS_DEF      => api.DefinitionType.ClassDef
      case APICallback.DefinitionType.TRAIT          => api.DefinitionType.Trait
      case APICallback.DefinitionType.MODULE         => api.DefinitionType.Module
      case APICallback.DefinitionType.PACKAGE_MODULE => api.DefinitionType.PackageModule
    }

    override def startClassLike(dt: Int, name: String, topLevel: Boolean): Unit = accept(StartClassLike(dt, name, topLevel))
    override def endClassLike(): Unit =
      val (structure: api.Lazy[Any]) ::
          (selfTpe: api.Lazy[Any]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          StartClassLike(dt, name, topLevel) :: stack1 = stack
      val definitionTpe = definitionType(dt)
      stack = api.ClassLike.create(
        name, access, mods, Array.empty, definitionTpe, selfTpe.asInstanceOf,
        structure.asInstanceOf, Array.empty, Array.empty, topLevel, Array.empty) :: stack1

    override def startClassLikeDef(dt: Int, name: String): Unit = accept(StartClassLikeDef(dt, name))
    override def endClassLikeDef(): Unit =
      val (mods: api.Modifiers) ::
          (access: api.Access) ::
          StartClassLikeDef(dt, name) :: stack1 = stack
      val definitionTpe = definitionType(dt)
      stack = api.ClassLikeDef.create(
        name, access, mods, Array.empty, Array.empty, definitionTpe) :: stack1

    override def registerSharedWith(id: Long): Unit = cache.put(id, stack.head)
    override def sharedValue(id: Long): Unit = accept(cache(id))

    override def saveNonLocalClass(): Unit =
      allNonLocalClassesInSrc += stack.head.asInstanceOf[api.ClassLike]
      stack = stack.tail

    override def startStructure(): Unit = ()
    override def endStructure(): Unit =
      val (inherited: api.Lazy[Any]) ::
          (decls: api.Lazy[Any]) ::
          (parents: api.Lazy[Any]) :: stack1 = stack
      stack = api.Structure.create(parents.asInstanceOf, decls.asInstanceOf, inherited.asInstanceOf) :: stack1

    override def endTypeSequence(): Unit =
      val (types, stack1) = peelFront[api.Type](_.isInstanceOf[api.Type])
      stack = types :: stack1

    override def endClassDefinitionSequence(): Unit =
      val (definitions, stack1) = peelFront[api.ClassDefinition](_.isInstanceOf[api.ClassDefinition])
      stack = definitions :: stack1

    override def startStrictLazy(): Unit = ()
    override def endStrictLazy(): Unit = modify(api.SafeLazy.strict)

    override def embedLazy(task: Runnable): Unit =
      val embedded = api.SafeLazy { () =>
        task.run()
        val result = stack.head
        stack = stack.tail
        result
      }
      accept(lzy(embedded))

    override def forceAllLazy(): Unit = forceThunks()

    override def startVal(name: String): Unit = accept(name)
    override def endVal(): Unit =
      val (retTpe: api.Type) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          (name: String) :: stack1 = stack
      stack = api.Val.create(name, access, mods, Array.empty, retTpe) :: stack1

    override def startDef(name: String): Unit = accept(name)
    override def endParamLists(): Unit =
      val (paramLists, stack1) = peelFront[api.ParameterList](_.isInstanceOf[api.ParameterList])
      stack = paramLists :: stack1
    override def endDef(): Unit =
      val (retTpe: api.Type) ::
          (paramss: Array[api.ParameterList]) ::
          (mods: api.Modifiers) ::
          (access: api.Access) ::
          (name: String) :: stack1 = stack
      stack = api.Def.create(name, access, mods, Array.empty, Array.empty, paramss, retTpe) :: stack1

    override def startParamList(isImplicit: Boolean): Unit = accept(isImplicit)
    override def endParamList(): Unit = modify((isImplicit: Boolean) => api.ParameterList.create(Array.empty, isImplicit))

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
