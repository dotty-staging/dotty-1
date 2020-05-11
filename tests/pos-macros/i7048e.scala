import scala.quoted._

abstract class Test {
  type T

  implicit val s: Scope

  val T: s.Type[T]
  val getT: s.Type[T] = T // need this to avoid getting `null`
  given getT.type = getT

  def foo: s.Expr[Any] = {

    val r = '{Option.empty[T]}

    {
      val t: Test = this
      import t.{given _}
      println(summon[t.s.Type[t.T]].show)
      // val r = '{Option.empty[t.T]} // access to value t from wrong staging level
      val r2 = '{Option.empty[${t.T}]}
    }

    {
      val r1 = '{Option.empty[${T}]} // works
      val r2 = '{Option.empty[List[${T}]]} // works
      // val r3 = '{summon[s.Type[${T}]]} // access to Test.this from wrong staging level
      val r4 = '{summon[${T} <:< Any]}
    }

    {
      val r0 = '{Option.empty[${T}]}
      val r1 = '{identity($r0)} // works
      val r2 = '{identity(${r0: s.Expr[Option[T]]})}
    }

    r
  }
}

@main def main = ()
