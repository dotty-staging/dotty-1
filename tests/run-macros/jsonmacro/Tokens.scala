package jsonmacro
package tokens

import result.*


object Tokens:
  private type ![T] = Result.Continuation[Parsed.Value, ParseError] ?=> T

class Tokens(source: Seq[String]):
  import Tokens.!

  /*private*/ var offset: Int = 0
  /*private*/ var part: Int = 0
  private var nextToken: Token = null

  skipWhiteSpaces()

  def atEnd(): ![Boolean] =
    skipWhiteSpaces()
    offset >= source(part).length

  def peek(): ![Token] =
    if nextToken ne null then nextToken
    else
      readNext()
      nextToken

  def next(): ![Token] =
    val res = peek()
    if nextToken ne Token.End then
      nextToken = null
    res

  def accept(token: Token): ![Unit] =
    val nextToken = next()
    if token != nextToken then error(s"expected token $token but got $nextToken")

  private def readNext(): ![Unit] =
    if offset == source(part).length then
      if part == source.length - 1 then
        offset += 1
        nextToken = Token.End
      else
        part += 1
        offset = 0
        nextToken = Token.InterpolatedValue
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
      case _ =>
        error(s"unexpected start of token")
    skipWhiteSpaces()

  private def peekChar(): ![Char] =
    source(part)(offset)

  private def nextChar(skipSpaces: Boolean = true): ![Char] =
    if offset >= source(part).length then error("unexpected end of JSON string 4")
    val char = source(part)(offset)
    offset += 1
    if skipSpaces then skipWhiteSpaces()
    char

  private def accept(char: Char): ![Unit] =
    if char != nextChar() then error(s"expected `$char`")

  private def accept(str: String): ![Unit] =
    for char <- str do
      if char != peekChar() then error(s"expected `$char`")
      nextChar(skipSpaces = false)
    if offset < source(part).length && !(source(part)(offset).isWhitespace || source(part)(offset) == '}' || source(part)(offset) == ']' || source(part)(offset) == ',') then error("expected end of token")
    skipWhiteSpaces()

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, part, offset))

  private def skipWhiteSpaces(): Unit =
    while part < source.length && offset < source(part).length && source(part)(offset).isWhitespace do
      offset += 1
