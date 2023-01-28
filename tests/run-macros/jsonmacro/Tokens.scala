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

  def peek(): ![Token] =
    if nextToken eq null then readNext()
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
    skipWhiteSpaces()
    nextToken =
      if offset == source(part).length then
        if part == source.length - 1 then
          offset += 1
          Token.End
        else
          part += 1
          offset = 0
          Token.InterpolatedValue
      else nextChar() match
        case '{' => Token.OpenBrace
        case '}' => Token.CloseBrace
        case '[' => Token.OpenBracket
        case ']' => Token.CloseBracket
        case 'n' => accept("ull"); Token.Null
        case 'f' => accept("alse"); Token.False
        case 't' => accept("rue"); Token.True
        case ',' => Token.Comma
        case ':' => Token.Colon
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
          ??? // Num(value: String)
        case '"' =>
          val stringBuffer = new collection.mutable.StringBuilder()
          def parseChars(): String =
            nextChar() match
              case '"' => stringBuffer.result()
              case '\\' =>
                nextChar() match
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
              case char if char.isControl =>
                error("unexpected control character")
              case char =>
                stringBuffer += char
                parseChars()
          Token.Str(parseChars())
        case _ =>
          error(s"unexpected start of token")

  private def accept(str: String): ![Unit] =
    for char <- str do
      if char != peekChar() then error(s"expected `$char`")
      offset += 1
    if offset < source(part).length && !(peekChar().isWhitespace || peekChar() == '}' || peekChar() == ']' || peekChar() == ',') then
      error("expected end of token")

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, part, offset))

  private def peekChar(): Char =
    source(part)(offset)

  private def nextChar(): Char =
    val char = peekChar()
    offset += 1
    char

  private def skipWhiteSpaces(): Unit =
    while part < source.length && offset < source(part).length && peekChar().isWhitespace do
      offset += 1
