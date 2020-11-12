package scala.quoted.internal

/** Implementation of scala.quoted.Type that sould only be extended by the implementation of `Quotes` */
abstract class Type[T <: AnyKind] extends scala.quoted.Type[T]
