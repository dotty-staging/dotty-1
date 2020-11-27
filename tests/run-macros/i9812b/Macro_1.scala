import quoted._
import SomeEnum._

trait Liftable[T] {
  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T): Quotes ?=> Expr[T]
}

object Lift:
  def apply[T: Liftable](t: T)(using q: Quotes, ev: Liftable[T]): Expr[T] = ev.toExpr(t)

sealed abstract class SomeEnum
object SomeEnum:
  final val Foo = new SomeEnum {}
  final case class Bar[S <: SomeEnum](s: S) extends SomeEnum
  object Bar:
    def apply[S <: SomeEnum](s: S): SomeEnum = new Bar(s)

given Liftable[Foo.type] as liftFoo:
  def toExpr(x: Foo.type): Quotes ?=> Expr[Foo.type] = '{Foo}

given [S <: SomeEnum: Type: Liftable] => Liftable[Bar[S]] as liftBar:
  def toExpr(x: Bar[S]): Quotes ?=> Expr[Bar[S]] = '{new Bar(${Lift(x.s)})}

sealed abstract class Lst[+T]
final case class CONS[+T](head: T, tail: Lst[T]) extends Lst[T]
case object NIL extends Lst[Nothing]

given [T <: Int] => Liftable[T] as intLiftable:
  def toExpr(x: T): Quotes ?=> Expr[T] = qctx ?=> {
    import quotes.reflect._
    Literal(Constant.Int(x)).asExpr.asInstanceOf[Expr[T]]
  }

given [T: Type: Liftable] => (ev1: => Liftable[CONS[T]], ev2: => Liftable[NIL.type]) => Liftable[Lst[T]] as liftLst:
  def toExpr(xs: Lst[T]): Quotes ?=> Expr[Lst[T]] = xs match
    case NIL               => ev2.toExpr(NIL)
    case CONS(_, _) as cons => ev1.toExpr(cons)

given [T: Type: Liftable] => (Liftable[Lst[T]]) => Liftable[CONS[T]] as liftCONS:
  def toExpr(x: CONS[T]): Quotes ?=> Expr[CONS[T]] = '{CONS(${Lift(x.head)}, ${Lift(x.tail)})}

given Liftable[NIL.type] as liftNIL:
  def toExpr(x: NIL.type): Quotes ?=> Expr[NIL.type] = '{NIL}

def mkLst[T](ts: T*) = ts.foldRight(NIL: Lst[T])(CONS(_,_))

def quote123: Quotes ?=> Expr[Lst[Int]] = Lift(mkLst(1,2,3))

inline def get123: Lst[Int] = ${ quote123 }

