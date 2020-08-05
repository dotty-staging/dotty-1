def myMacroImpl(a: quoted.Expr[_])(using qctx: quoted.QuoteContext) = {
  import qctx.tasty._
  def typed[A] = {
    implicit val t: quoted.Type[A] = a.asTerm.tpe.widen.seal.asInstanceOf[quoted.Type[A]]
    '{
      type T = $t
      ${a.asTerm.asExprOf[T]}
    }
  }

  typed
}


inline transparent def myMacro(a: => Any) = ${
  myMacroImpl('a)
}
