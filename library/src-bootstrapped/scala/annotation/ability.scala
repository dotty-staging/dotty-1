package scala.annotation

/** An annotation inidcating that a val should be tracked as its own ability.
 *  Example:
 *
 *   @ability erased val canThrow: * = ???
 */
class ability extends StaticAnnotation