package dotty.tools.dotc.sbt

import java.nio.file.{Paths, Path}

import org.junit.Assert._
import org.junit.{Test => test, BeforeClass => beforeClass, AfterClass => afterClass}
import org.junit.experimental.categories.Category

import scala.util.chaining._
import scala.collection.mutable

import dotty.BootstrappedOnlyTests
import dotty.tools.dotc.Main
import dotty.tools.dotc.sbt.APICallback
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, FreshContext}

@Category(Array(classOf[BootstrappedOnlyTests]))
class APICallbackTests:
  import xsbti.api.{DefinitionType => dt}
  import APICallbackTests._
  import APIEvent._

  val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.sbt.test"))

  @test def helloWorld: Unit = assertEquals(
    List(
      StartSource(path=rootSrc.resolve("HelloWorld.scala")),
        StartClassLikeDef(dt.ClassDef, name="example.HelloWorld"),
          Public,
          Modifiers(isFinal=true),
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
        EndClassLikeDef,
      EndSource
    ),
    compileOnly("HelloWorld.scala").events,
  )

  private def compileOnly(src: String): APIEventHolder =
    compile(rootSrc.resolve(src) :: Nil)

  private def compile(srcs: List[Path]): APIEventHolder =
    val (eventHolder, rootCtx) = setupEventHolder
    val exit = Main.process(args(srcs), rootCtx)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    eventHolder

end APICallbackTests

object APICallbackTests:
  import APIEvent._
  import xsbti.api

  enum APIEvent:
    case StartSource(path: Path)
    case EndSource
    case StartClassLikeDef(definitionType: api.DefinitionType, name: String)
    case EndClassLikeDef
    case StartVal(name: String)
    case EndVal
    case StartDef(name: String)
    case EndDef
    case StartParamList(isImplicit: Boolean)
    case EndParamList
    case StartConstant(value: Any)
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

    override def startClassLikeDef(dt: api.DefinitionType, name: String): Unit = accept(StartClassLikeDef(dt, name))
    override def endClassLikeDef(): Unit = accept(EndClassLikeDef)

    override def startVal(name: String): Unit = accept(StartVal(name))
    override def endVal(): Unit = accept(EndVal)

    override def startDef(name: String): Unit = accept(StartDef(name))
    override def endDef(): Unit = accept(EndDef)

    override def startParamList(isImplicit: Boolean): Unit = accept(StartParamList(isImplicit))
    override def endParamList(): Unit = accept(EndParamList)

    override def startProjection(selected: String): Unit = accept(StartProjection(selected))
    override def endProjection(): Unit = accept(EndProjection)

    override def startConstant(constant: Any): Unit = accept(StartConstant(constant))
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

  // Alternative to above: make a new callback for each context that selects which overrides to implement

  private var _initialCtx: Context = null

  private val commonArgs = Array(
    "-Ystop-after:api-callback",
    "-usejavacp"
  )

  def args(srcs: List[Path]) = commonArgs ++ srcs.map(_.toString)

  @beforeClass def setup: Unit =
    _initialCtx = ContextBase().initialCtx

  @afterClass def cleanup: Unit =
    _initialCtx = null

  def setupEventHolder: (APIEventHolder, FreshContext) =
    val cb = new TestAPICallback with APIEventHolder
    cb -> _initialCtx.fresh.setAPICallback(cb)

end APICallbackTests
