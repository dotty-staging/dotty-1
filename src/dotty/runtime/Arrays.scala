package dotty.runtime

import scala.reflect.ClassTag

/** All but the first operation should be short-circuited and implemented specially by
 *  the backend.
 */
object Arrays {

  /** Creates an array of some element type determined by the given `ClassTag`
   *  argument. The erased type of applications of this method is `Object`.
   */
  def newGenericArray[T](length: Int)(implicit tag: ClassTag[T]): Array[T] =
    tag.newArray(length)

  /** Create a Object[] array. The translation scheme ensures that the erased type
   *  of all applications of this method is `Object[]` (instead of `Object`, as the
   *  result type would imply).
   */
  def newRefArray[T](length: Int): T = ???

  /** Create a Byte[] array */
  def newByteArray(length: Int): Array[Byte] = ???

  /** Create a Short[] array */
  def newShortArray(length: Int): Array[Short] = ???

  /** Create a Char[] array */
  def newCharArray(length: Int): Array[Char] = ???

  /** Create an Int[] array */
  def newIntArray(length: Int): Array[Int] = ???

  /** Create a Long[] array */
  def newLongArray(length: Int): Array[Long] = ???

  /** Create a Float[] array */
  def newFloatArray(length: Int): Array[Float] = ???

  /** Create a Double[] array */
  def newDoubleArray(length: Int): Array[Double] = ???

  /** Create a Boolean[] array */
  def newBooleanArray(length: Int): Array[Boolean] = ???

  /** Create a scala.runtime.BoxedUnit[] array */
  def newUnitArray(length: Int): Array[Unit] = ???
}