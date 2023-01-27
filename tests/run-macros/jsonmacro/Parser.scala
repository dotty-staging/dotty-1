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

  private type ![T] = Result.Continuation[interpolated.Value, ParseError] ?=> T

  private var offset = 0

  def parse(): Result[interpolated.Value, ParseError] =
    skipWhiteSpaces()
    Result.withContinuation {
      val parsed = parseValue()
      if offset < source.length then error("value ended")
      else parsed
    }

  private def peek(): ![Char] =
    if offset >= source.length then error("unexpected end of JSON string")
    source(offset)

  private def next(skipSpaces: Boolean = true): ![Char] =
    if offset >= source.length then error("unexpected end of JSON string")
    val char = source(offset)
    offset += 1
    if skipSpaces then skipWhiteSpaces()
    char

  private def accept(char: Char): ![Unit] =
    if char != next() then error(s"expected `$char`")

  private def accept(str: String): ![Unit] =
    for char <- str do
      if char != peek() then error(s"expected `$char`")
      next(skipSpaces = false)
    if offset < source.length && !(source(offset).isWhitespace || source(offset) == '}' || source(offset) == ']' || source(offset) == ',') then error("expected end of token")
    skipWhiteSpaces()

  private def skipWhiteSpaces(): Unit =
    while offset < source.length && source(offset).isWhitespace do
      offset += 1

  private def parseValue(): ![interpolated.Value] =
    peek() match
      case '{' => parseObject()
      case '[' => parseArray()
      case '"' => parseString()
      case 't' => parseTrue()
      case 'f' => parseFalse()
      case 'n' => parseNull()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  => parseNumber()
      case char => error(s"expected JSON value")

  private def parseObject(): ![interpolated.Obj] =
    accept('{')
    val nameValues =
      if peek() == '}' then Vector.empty
      else commaSeparatedNameValues()
    accept('}')
    // TODO validate key duplication
    interpolated.Obj(Map(nameValues*))

  private def commaSeparatedNameValues(): ![Vector[(interpolated.Str, interpolated.Value)]] =
    def parseNext(values: Vector[(interpolated.Str, interpolated.Value)]): Vector[(interpolated.Str, interpolated.Value)] =
      peek() match
        case ',' =>
          accept(',')
          parseNext(values :+ parseNameValue())
        case _ => values
    parseNext(Vector(parseNameValue()))

  private def parseNameValue(): ![(interpolated.Str, interpolated.Value)] =
    val name = parseString()
    accept(':')
    name -> parseValue()

  private def parseArray(): ![interpolated.Arr] =
    accept('[')
    val values =
      if peek() == ']' then Vector.empty
      else commaSeparatedValues()
    accept(']')
    interpolated.Arr(values*)

  private def commaSeparatedValues(): ![Vector[interpolated.Value]] =
    def parseNext(values: Vector[interpolated.Value]): Vector[interpolated.Value] =
      peek() match
        case ',' =>
          accept(',')
          parseNext(values :+ parseValue())
        case _ => values
    parseNext(Vector(parseValue()))

  private def parseString(): ![interpolated.Str] =
    accept('"')
    val stringBuffer = new collection.mutable.StringBuilder()
    def parseChars(): String =
      next(skipSpaces = false) match
        case '"' => stringBuffer.result()
        case '\\' =>
          next(skipSpaces = false) match
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
    interpolated.Str(parseChars())

  private def parseNumber(): ![interpolated.Num] =
    ???

  private def parseTrue(): ![interpolated.Bool] =
    accept("true")
    interpolated.Bool(true)

  private def parseFalse(): ![interpolated.Bool] =
    accept("false")
    interpolated.Bool(false)

  private def parseNull(): ![interpolated.Null.type] =
    accept("null")
    interpolated.Null

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, offset))