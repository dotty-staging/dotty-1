package jsonlib.parser

import scala.util.boundary

import jsonlib.util.*

private class Tokens(source: Seq[String]):

  /*private*/ var offset: Int = 0
  /*private*/ var part: Int = 0
  private var nextToken: Token = null

  def peek(): Token =
    if nextToken eq null then readNext()
    nextToken

  def next(): Token =
    val res = peek()
    if nextToken ne Token.End then
      nextToken = null
    res

  private def readNext(): Unit =
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
        case 'n' => accept("null", Token.Null)
        case 'f' => accept("false", Token.False)
        case 't' => accept("true", Token.True)
        case ',' => Token.Comma
        case ':' => Token.Colon
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
          ??? // Num(value: String)
        case '"' =>
          boundary {
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
                  boundary.break(Token.Error("unexpected control character", part, offset))
                case char =>
                  stringBuffer += char
                  parseChars()
            Token.Str(parseChars())
          }
        case _ =>
          Token.Error("unexpected start of token", part, offset)

  private def accept(str: String, token: Token): Token =
    for char <- str.tail do
      if char != peekChar() then
        return Token.Error(s"expected `$char` (of $str)", part, offset)
      offset += 1
    if offset < source(part).length && !(peekChar().isWhitespace || peekChar() == '}' || peekChar() == ']' || peekChar() == ',') then
      Token.Error("expected end of token", part, offset)
    else
      token

  private def peekChar(): Char =
    source(part)(offset)

  private def nextChar(): Char =
    val char = peekChar()
    offset += 1
    char

  private def skipWhiteSpaces(): Unit =
    while part < source.length && offset < source(part).length && (peekChar().isWhitespace || peekChar() == '\n') do
      offset += 1
