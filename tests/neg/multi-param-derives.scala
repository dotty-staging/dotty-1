import scala.deriving._

object Test extends App {
  {
    trait Show[T]
    object Show {
      given Show[Int] {}
      given [T](using st: Show[T]) as Show[Tuple1[T]] {}
      given t2[T, U](using st: Show[T], su: Show[U]) as Show[(T, U)] {}
      given t3[T, U, V](using st: Show[T], su: Show[U], sv: Show[V]) as Show[(T, U, V)] {}

      def derived[T](using m: Mirror.Of[T], r: Show[m.MirroredElemTypes]): Show[T] = new Show[T] {}
    }

    case class Mono(i: Int) derives Show
    case class Poly[A](a: A) derives Show
    case class Poly11[F[_]](fi: F[Int]) derives Show // error
    case class Poly2[A, B](a: A, b: B) derives Show
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Show
  }

  {
    trait Functor[F[_]]
    object Functor {
      given [C] as Functor[[T] =>> C] {}
      given Functor[[T] =>> Tuple1[T]] {}
      given t2 [T] as Functor[[U] =>> (T, U)] {}
      given t3 [T, U] as Functor[[V] =>> (T, U, V)] {}

      def derived[F[_]](using m: Mirror { type MirroredType[X] = F[X] ; type MirroredElemTypes[_] }, r: Functor[m.MirroredElemTypes]): Functor[F] = new Functor[F] {}
    }

    case class Mono(i: Int) derives Functor
    case class Poly[A](a: A) derives Functor
    case class Poly11[F[_]](fi: F[Int]) derives Functor // error
    case class Poly2[A, B](a: A, b: B) derives Functor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Functor
  }

  {
    trait FunctorK[F[_[_]]]
    object FunctorK {
      given [C] as FunctorK[[F[_]] =>> C] {}
      given [T] as FunctorK[[F[_]] =>> Tuple1[F[T]]]

      def derived[F[_[_]]](using m: Mirror { type MirroredType[X[_]] = F[X] ; type MirroredElemTypes[_[_]] }, r: FunctorK[m.MirroredElemTypes]): FunctorK[F] = new FunctorK[F] {}
    }

    case class Mono(i: Int) derives FunctorK
    case class Poly[A](a: A) derives FunctorK // error
    case class Poly11[F[_]](fi: F[Int]) derives FunctorK
    case class Poly2[A, B](a: A, b: B) derives FunctorK // error
    case class Poly3[A, B, C](a: A, b: B, c: C) derives FunctorK // error
  }

  {
    trait Bifunctor[F[_, _]]
    object Bifunctor {
      given [C] => Bifunctor[[T, U] =>> C] {}
      given Bifunctor[[T, U] =>> Tuple1[U]] {}
      given Bifunctor[[T, U] =>> (T, U)] as t2 {}
      given t3 [T] as Bifunctor[[U, V] =>> (T, U, V)] {}

      def derived[F[_, _]](using m: Mirror { type MirroredType[X, Y] = F[X, Y] ; type MirroredElemTypes[_, _] }, r: Bifunctor[m.MirroredElemTypes]): Bifunctor[F] = ???
    }

    case class Mono(i: Int) derives Bifunctor
    case class Poly[A](a: A) derives Bifunctor
    case class Poly11[F[_]](fi: F[Int]) derives Bifunctor // error
    case class Poly2[A, B](a: A, b: B) derives Bifunctor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Bifunctor
  }
}

