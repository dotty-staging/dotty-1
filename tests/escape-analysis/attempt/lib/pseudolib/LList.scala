package pseudolib

import annotation.internal.local

sealed abstract class LList[T] {
  def ::(t: T): LList[T] = CCons(t, this)
}
final case class CCons[T](head: T, tail: LList[T]) extends LList[T]
final case class NNil[T]() extends LList[T]

object LList {

  def map[T, U](l: LList[T])(@local f: T => U): LList[U] =
    l match {
      case CCons(h, t) =>
        CCons(f(h), map(t)(f))
      case NNil => NNil[U]()
    }

//  def map[T, U](l: LList[T])(@local f: T => U): LList[U] =
//    CCons(f, NNil()).asInstanceOf[LList[U]]

}
