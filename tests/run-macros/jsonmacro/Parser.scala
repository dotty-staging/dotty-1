package jsonmacro

import scala.quoted.*

import result.*

object interpolated:
  sealed trait Value
  final case class Obj(value: Map[Str, Value]) extends Value
  final case class Arr(values: Value*) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  final case class Bool(value: Boolean) extends Value
  case object Null extends Value
  final case class Interpolated(idx: Int) extends Value

final case class ParseError(msg: String, offset: Int)
private class Parser(source: String):

  private val tokens = new Tokens(source)

  private type ![T] = Result.Continuation[interpolated.Value, ParseError] ?=> T

  def parse(): Result[interpolated.Value, ParseError] =
    Result.withContinuation {
      val parsed = parseValue()
      if !tokens.atEnd() then error("value ended")
      else parsed
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
    Result.continuation.error(ParseError(msg, tokens.offset))

enum Token:
  case Null
  case False
  case True
  case Num(value: String)
  case OpenBrace
  case CloseBrace
  case OpenBracket
  case CloseBracket
  case Comma
  case Colon
  case Str(value: String)

object Tokens:
  private type ![T] = Result.Continuation[interpolated.Value, ParseError] ?=> T

class Tokens(source: String):
  import Tokens.!

  /*private*/ var offset: Int = 0
  private var nextToken: Token = null

  skipWhiteSpaces()

  def atEnd(): ![Boolean] =
    skipWhiteSpaces()
    offset >= source.length

  def peek(): ![Token] =
    if nextToken ne null then nextToken
    else
      readNext()
      nextToken

  def next(): ![Token] =
    val res = peek()
    nextToken = null
    res

  private def readNext(): ![Unit] =
    if offset >= source.length then error("unexpected end of JSON string")
    else peekChar() match
      case '{' =>
        accept('{')
        nextToken = Token.OpenBrace
      case '}' =>
        accept('}')
        nextToken = Token.CloseBrace
      case '[' =>
        accept('[')
        nextToken = Token.OpenBracket
      case ']' =>
        accept(']')
        nextToken = Token.CloseBracket
      case 'n' =>
        accept("null")
        nextToken = Token.Null
      case 'f' =>
        accept("false")
        nextToken = Token.False
      case 't' =>
        accept("true")
        nextToken = Token.True
      case ',' =>
        accept(',')
        nextToken = Token.Comma
      case ':' =>
        accept(':')
        nextToken = Token.Colon
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
        nextToken = ??? // Num(value: String)
      case '"' =>
        accept('"')
        val stringBuffer = new collection.mutable.StringBuilder()
        def parseChars(): String =
          nextChar(skipSpaces = false) match
            case '"' => stringBuffer.result()
            case '\\' =>
              nextChar(skipSpaces = false) match
                case '\\' => stringBuffer += '\\'
                case '"' => stringBuffer += '"'
                case '/' => stringBuffer += '/'
                case 'b' => stringBuffer += '\b'
                case 'f' => stringBuffer += '\f'
                case 'n' => stringBuffer += '\n'
                case 'r' => stringBuffer += '\r'
                case 't' => stringBuffer += '\t'
                case 'u' => ??? // 4 hexadecimal digits
              parseChars()
            case char if char.isControl => error("unexpected control character")
            case char =>
              stringBuffer += char
              parseChars()
        nextToken = Token.Str(parseChars())
      case _ => error(s"expected token")
    skipWhiteSpaces()

  private def peekChar(): ![Char] =
    if offset >= source.length then error("unexpected end of JSON string")
    source(offset)

  private def nextChar(skipSpaces: Boolean = true): ![Char] =
    if offset >= source.length then error("unexpected end of JSON string")
    val char = source(offset)
    offset += 1
    if skipSpaces then skipWhiteSpaces()
    char

  private def accept(char: Char): ![Unit] =
    if char != nextChar() then error(s"expected `$char`")

  private def accept(str: String): ![Unit] =
    for char <- str do
      if char != peekChar() then error(s"expected `$char`")
      nextChar(skipSpaces = false)
    if offset < source.length && !(source(offset).isWhitespace || source(offset) == '}' || source(offset) == ']' || source(offset) == ',') then error("expected end of token")
    skipWhiteSpaces()

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, offset))

  private def skipWhiteSpaces(): Unit =
    while offset < source.length && source(offset).isWhitespace do
      offset += 1
