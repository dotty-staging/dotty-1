import scala.quoted._
def coroutineImpl(using s: Scope): s.Expr[Any] =
  '{
    new {
      def state: Int = 0
      ${identity('state)}
    }
  }
