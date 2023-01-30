package jsonlib.parser

import jsonlib.util.*

object Parsed:
  sealed trait Value
  final case class Obj(nameValues: (String, Value)*) extends Value
  final case class Arr(values: Value*) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  final case class Bool(value: Boolean) extends Value
  case object Null extends Value
  final case class InterpolatedValue(idx: Int) extends Value

final case class ParseError(msg: String, part: Int, offset: Int)
class Parser(source: Seq[String]):

  private val tokens = new Tokens(source)
  private var interpolationsIndex = -1

  private type ![T] = Result.Continuation[Parsed.Value, ParseError] ?=> T

  def parse(): Result[Parsed.Value, ParseError] =
    Result.withContinuation {
      val parsed = parseValue()
      accept(Token.End)
      parsed
    }

  private def parseValue(): ![Parsed.Value] =
    tokens.next() match
      case Token.Null => Parsed.Null
      case Token.True => Parsed.Bool(true)
      case Token.False => Parsed.Bool(false)
      case Token.Str(value) => Parsed.Str(value)
      case Token.Num(value) => Parsed.Num(???)
      case Token.OpenBrace =>
        val nameValues =
          if tokens.peek() == Token.CloseBrace then Vector.empty
          else commaSeparate(parseNameValue)
        accept(Token.CloseBrace)
        nameValues.map(_._1).groupBy(x => x).filter(_._2.length > 1).foreach { x =>
          error(s"Duplicate name: ${x._1}")
        }
        Parsed.Obj(nameValues*)
      case Token.OpenBracket =>
        val values =
          if tokens.peek() == Token.CloseBracket then Vector.empty
          else commaSeparate(parseValue)
        accept(Token.CloseBracket)
        Parsed.Arr(values*)
      case Token.InterpolatedValue =>
        interpolationsIndex += 1
        Parsed.InterpolatedValue(interpolationsIndex)
      case token =>
        error(s"unexpected start of value: $token")

  private def commaSeparate[T](parseItem: () => ![T]): ![Vector[T]] =
    def parseNext(values: Vector[T]): Vector[T] =
      tokens.peek() match
        case Token.Comma =>
          accept(Token.Comma)
          parseNext(values :+ parseItem())
        case _ => values
    parseNext(Vector(parseItem()))

  private def parseNameValue(): ![(String, Parsed.Value)] =
    tokens.next() match
      case Token.Str(value) =>
        accept(Token.Colon)
        value -> parseValue()
      case _ =>
        error("expected string literal")

  private def accept(token: Token): ![Unit] =
    val nextToken = tokens.next()
    if token != nextToken then error(s"expected token $token but got $nextToken")

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, tokens.part, tokens.offset))
