package jsonmacro

import scala.util.boundary

object Parser:
  sealed trait Result
  final case class Parsed(json: Json.Value) extends Result
  final case class Error(msg: String, offset: Int) extends Result

  private type ![T] = boundary.Label[Error] ?=> T

  private def handleParseErrors(x: ![Json.Value]): Result =
    boundary { Parsed(x) }

private class Parser(source: String):
  import Parser.*

  private var offset = 0

  def parse(): Result =
    skipWhiteSpaces()
    handleParseErrors {
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

  private def parseValue(): ![Json.Value] =
    peek() match
      case '{' => parseObject()
      case '[' => parseArray()
      case '"' => parseString()
      case 't' => parseTrue()
      case 'f' => parseFalse()
      case 'n' => parseNull()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  => parseNumber()
      case char => error(s"expected JSON value")

  private def parseObject(): ![Json.Obj] =
    accept('{')
    val nameValues =
      if peek() == '}' then Vector.empty
      else commaSeparatedNameValues()
    accept('}')
    // TODO validate key duplication
    Json.Obj(Map(nameValues*))

  private def commaSeparatedNameValues(): ![Vector[(Json.Str, Json.Value)]] =
    def parseNext(values: Vector[(Json.Str, Json.Value)]): Vector[(Json.Str, Json.Value)] =
      peek() match
        case ',' =>
          accept(',')
          parseNext(values :+ parseNameValue())
        case _ => values
    parseNext(Vector(parseNameValue()))

  private def parseNameValue(): ![(Json.Str, Json.Value)] =
    val name = parseString()
    accept(':')
    name -> parseValue()

  private def parseArray(): ![Json.Arr] =
    accept('[')
    val values =
      if peek() == ']' then Vector.empty
      else commaSeparatedValues()
    accept(']')
    Json.Arr(values*)

  private def commaSeparatedValues(): ![Vector[Json.Value]] =
    def parseNext(values: Vector[Json.Value]): Vector[Json.Value] =
      peek() match
        case ',' =>
          accept(',')
          parseNext(values :+ parseValue())
        case _ => values
    parseNext(Vector(parseValue()))

  private def parseString(): ![Json.Str] =
    accept('"')
    val stringBuffer = new collection.mutable.StringBuilder()
    def parseChars(): Json.Str =
      next(skipSpaces = false) match
        case '"' => Json.Str(stringBuffer.result())
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
    parseChars()

  private def parseNumber(): ![Json.Num] =
    ???

  private def parseTrue(): ![Json.Bool] =
    accept("true")
    Json.Bool(true)

  private def parseFalse(): ![Json.Bool] =
    accept("false")
    Json.Bool(false)

  private def parseNull(): ![Json.Null.type] =
    accept("null")
    Json.Null

  private def error(msg: String): ![Nothing] =
    boundary.break(Error(msg, offset))