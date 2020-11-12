package dotty.internal

import scala.quoted._

object CompileTimeMacros:
  def codeExpr(using qctx: Quotes)(sc: Expr[StringContext], args: Expr[Seq[Any]]): Expr[String] =
    throw new Exception("Non bootstrapped library")
