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
      StartSource(rootSrc.resolve("HelloWorld.scala")),
      StartClassLike(dt.ClassDef, "example.HelloWorld"),
      EndClassLike,
      EndSource
    ),
    compileOnly("HelloWorld.scala"),
  )

  private def compileOnly(src: String): List[APIEvent] =
    compile(rootSrc.resolve(src) :: Nil)

  private def compile(srcs: List[Path]): List[APIEvent] =
    val (apiCallback, rootCtx) = setupCallback
    val exit = Main.process(args(srcs), rootCtx)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    apiCallback.events

end APICallbackTests

object APICallbackTests:
  import APIEvent._
  import xsbti.api.DefinitionType

  enum APIEvent:
    case StartSource(path: Path)
    case StartClassLike(definitionType: DefinitionType, name: String)
    case EndSource
    case EndClassLike

  trait APIEventHolder:
    private val _events = mutable.ArrayBuffer.empty[APIEvent]
    protected def push(event: APIEvent): Unit = _events += event
    def events: List[APIEvent] = _events.toList
  end APIEventHolder

  final class TestAPICallback extends APICallback with APIEventHolder:

    override def startSource(src: Path): Unit =
      push(StartSource(src))

    override def startClassLike(dt: DefinitionType, name: String): Unit =
      push(StartClassLike(dt, name))

    override def endSource(): Unit = push(EndSource)
    override def endClassLike(): Unit = push(EndClassLike)

  end TestAPICallback

  private var _initialCtx: Context = null

  private val commonArgs = Array(
    "-Yforce-sbt-phases",
    "-Ystop-after:sbt-api",
    "-usejavacp"
  )

  def args(srcs: List[Path]) = commonArgs ++ srcs.map(_.toString)

  @beforeClass def setup: Unit =
    _initialCtx = ContextBase().initialCtx

  @afterClass def cleanup: Unit =
    _initialCtx = null

  def setupCallback: (TestAPICallback, FreshContext) =
    TestAPICallback().pipe(cb => cb -> _initialCtx.fresh.setAPICallback(cb))

end APICallbackTests
