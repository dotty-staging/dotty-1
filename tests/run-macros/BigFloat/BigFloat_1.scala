package test
import scala.util.FromDigits
import scala.quoted._


case class BigFloat(mantissa: BigInt, exponent: Int) {
  override def toString = s"${mantissa}e${exponent}"
}

object BigFloat extends App {
  def apply(digits: String): BigFloat = {
    val (mantissaDigits, givenExponent) = digits.toUpperCase.split('E') match {
      case Array(mantissaDigits, edigits) =>
        val expo =
          try FromDigits.intFromDigits(edigits)
          catch {
            case ex: FromDigits.NumberTooLarge =>
              throw FromDigits.NumberTooLarge(s"exponent too large: $edigits")
          }
        (mantissaDigits, expo)
      case Array(mantissaDigits) =>
        (mantissaDigits, 0)
    }
    val (intPart, exponent) = mantissaDigits.split('.') match {
      case Array(intPart, decimalPart) =>
        (intPart ++ decimalPart, givenExponent - decimalPart.length)
      case Array(intPart) =>
        (intPart, givenExponent)
    }
    BigFloat(BigInt(intPart), exponent)
  }

  class BigFloatFromDigits extends FromDigits.Floating[BigFloat] {
    def fromDigits(digits: String) = apply(digits)
  }

  given BigFloatFromDigits {
    override inline def fromDigits(digits: String) = ${
      BigFloatFromDigitsImpl('digits)
    }
  }

  // Should be in StdLib:

  given (using s: Scope) as s.Liftable[BigInt] {
    def toExpr(x: BigInt) =
      '{BigInt(${Expr(x.toString)})}
  }
}

