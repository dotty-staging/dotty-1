package jsonmacro

import result.*
import tokens.*

object interpolated:
  sealed trait Value
  final case class Obj(value: Map[Str, Value]) extends Value
  final case class Arr(values: Value*) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  final case class Bool(value: Boolean) extends Value
  case object Null extends Value
  case object InterpolatedValue extends Value

final case class ParseError(msg: String, part: Int, offset: Int)
private class Parser(source: Seq[String]):

  private val tokens = new Tokens(source)

  private type ![T] = Result.Continuation[interpolated.Value, ParseError] ?=> T

  def parse(): Result[interpolated.Value, ParseError] =
    Result.withContinuation {
      val parsed = parseValue()
      accept(Token.End)
      parsed
    }

  private def accept(token: Token): ![Unit] =
    val next = tokens.next()
    if token != next then error(s"expected token $token but got $next")

  private def parseValue(): ![interpolated.Value] =
    tokens.peek() match
      case Token.OpenBrace => parseObject()
      case Token.OpenBracket => parseArray()
      case Token.Str(value) => tokens.next(); interpolated.Str(value)
      case Token.True => tokens.next(); interpolated.Bool(true)
      case Token.False => tokens.next(); interpolated.Bool(false)
      case Token.Null => tokens.next(); interpolated.Null
      case Token.Num(value) => tokens.next(); interpolated.Num(???)
      case Token.InterpolatedValue => tokens.next(); interpolated.InterpolatedValue
      case token => error(s"unexpected token starting a value: $token")

  private def parseObject(): ![interpolated.Obj] =
    accept(Token.OpenBrace)
    val nameValues =
      if tokens.peek() == Token.CloseBrace then Vector.empty
      else commaSeparatedNameValues()
    accept(Token.CloseBrace)

    // TODO validate key duplication
    interpolated.Obj(Map(nameValues*))

  private def commaSeparatedNameValues(): ![Vector[(interpolated.Str, interpolated.Value)]] =
    def parseNext(values: Vector[(interpolated.Str, interpolated.Value)]): Vector[(interpolated.Str, interpolated.Value)] =
      tokens.peek() match
        case Token.Comma =>
          accept(Token.Comma)
          parseNext(values :+ parseNameValue())
        case _ => values
    parseNext(Vector(parseNameValue()))

  private def parseNameValue(): ![(interpolated.Str, interpolated.Value)] =
    tokens.next() match
      case Token.Str(value) =>
        accept(Token.Colon)
        interpolated.Str(value) -> parseValue()
      case _ =>
        error("expected string literal")

  private def parseArray(): ![interpolated.Arr] =
    accept(Token.OpenBracket)
    val values =
      if tokens.peek() == Token.CloseBracket then Vector.empty
      else commaSeparatedValues()
    accept(Token.CloseBracket)
    interpolated.Arr(values*)

  private def commaSeparatedValues(): ![Vector[interpolated.Value]] =
    def parseNext(values: Vector[interpolated.Value]): Vector[interpolated.Value] =
      tokens.peek() match
        case Token.Comma =>
          accept(Token.Comma)
          parseNext(values :+ parseValue())
        case _ => values
    parseNext(Vector(parseValue()))

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, tokens.part, tokens.offset))

