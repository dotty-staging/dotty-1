package scala.annotation

/** An annotation on a parameter type that allows implicit conversions
 *  for its arguments. Intended for use by Scala 2, to annotate Scala 2
 *  libraries. Scala 3 uses the `convertibleTo` modifier on the parameter
 *  type instead.
 */
class allowConversions extends scala.annotation.StaticAnnotation
