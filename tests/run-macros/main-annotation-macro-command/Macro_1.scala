import scala.concurrent._
import scala.annotation.*
import scala.quoted.*
import scala.collection.mutable
import ExecutionContext.Implicits.global
import duration._
import MainAnnotation.CommandInfo



@experimental
class simpleMain extends MainAnnotation:
  import simpleMain.*
  inline def command(inline info: CommandInfo, args: Array[String]): Command = ${ commandExpr('info, 'args) }

object simpleMain:
  def commandExpr(infoExpr: Expr[CommandInfo], argsExpr: Expr[Array[String]])(using Quotes): Expr[Command] =
    val info = infoExpr.valueOrAbort
    println("++++++++++++")
    println(infoExpr.show)
    println()
    println(info)
    println()
    println()
    '{ new Command($argsExpr) }

  class Command(args: Array[String]):
    @compileTimeOnly("@simpleMain must have String* arguments")
    def argGetter[T](idx: Int, defaultArgument: Option[() => T]): () => String = ???
    def varargGetter[T]: () => Seq[String] = () => args
    def run(f: () => Unit): Unit = f()

