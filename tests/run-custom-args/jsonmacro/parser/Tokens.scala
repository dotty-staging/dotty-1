package jsonlib.parser

import scala.util.boundary

import jsonlib.util.*

private class Tokens(source: Seq[String]):

  /*private*/ var offset: Int = 0
  /*private*/ var part: Int = 0
  private var nextToken: Token = null

  def peek(): Token =
    if nextToken eq null then
      nextToken = readToken()
    nextToken

  def next(): Token =
    val res = peek()
    if nextToken ne Token.End then
      nextToken = null
    res

  private def readToken(): Token =
    skipWhiteSpaces()
    if offset == source(part).length then
      if part == source.length - 1 then
        offset += 1
        Token.End
      else
        part += 1
        offset = 0
        Token.InterpolatedValue
    else peekChar() match
      case '{' => accept('{', Token.OpenBrace)
      case '}' => accept('}', Token.CloseBrace)
      case '[' => accept('[', Token.OpenBracket)
      case ']' => accept(']', Token.CloseBracket)
      case 'n' => accept("null", Token.Null)
      case 'f' => accept("false", Token.False)
      case 't' => accept("true", Token.True)
      case ',' => accept(',', Token.Comma)
      case ':' => accept(':', Token.Colon)
      case '"' => readString()
      case '-' => readNum()
      case c if '0' <= c && c <= '9' => readNum()
      case _ =>
        Token.Error(s"unexpected start of token ${nextChar()}", part, offset)

  private def readString(): Token =
    assert(nextChar() == '"')
    boundary:
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
              case 'u' => Token.Error("HEX not supported", part, offset) // TODO support 4 hexadecimal digits
            parseChars()
          case char if char.isControl =>
            boundary.break(Token.Error("unexpected control character", part, offset))
          case char =>
            stringBuffer += char
            parseChars()
      Token.Str(parseChars())

  private def readNum(): Token =
    Token.Error("JSON number not supported", part, offset) // TODO support numbers

  private def accept(char: Char, token: Token): Token =
    nextChar() match
      case `char` => token
      case next => Token.Error(s"unexpected character: got $char but got $next", part, offset)

  private def accept(str: String, token: Token): Token =
    for char <- str do
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
