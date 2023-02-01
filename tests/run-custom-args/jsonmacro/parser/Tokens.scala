package jsonlib.parser

import scala.util.boundary

import jsonlib.util.*

private class Tokens(chars: InterpolatedChars):

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

  def location: Location = chars.location

  private def readToken(): Token =
    boundary:
      chars.skipWhiteSpaces()
      if chars.atEnd then
        Token.End
      else if chars.atInterpolation then
        chars.nextPart()
        Token.InterpolatedValue
      else chars.peekChar() match
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
          Token.Error(s"unexpected start of token ${chars.nextChar()}", chars.location)

  private def readString()(using boundary.Label[Token]): Token =
    assert(chars.nextChar() == '"')
    val stringBuffer = new collection.mutable.StringBuilder()
    def parseChars(): String =
      nextCharOrError() match
        case '"' => stringBuffer.result()
        case '\\' =>
          nextCharOrError() match
            case '\\' => stringBuffer += '\\'
            case '"' => stringBuffer += '"'
            case '/' => stringBuffer += '/'
            case 'b' => stringBuffer += '\b'
            case 'f' => stringBuffer += '\f'
            case 'n' => stringBuffer += '\n'
            case 'r' => stringBuffer += '\r'
            case 't' => stringBuffer += '\t'
            case 'u' => boundary.break(Token.Error("HEX not supported", chars.location)) // TODO support 4 hexadecimal digits
          parseChars()
        case char if char.isControl =>
          boundary.break(Token.Error("unexpected control character", chars.location))
        case char =>
          stringBuffer += char
          parseChars()
    Token.Str(parseChars())

  private def readNum(): Token =
    Token.Error("JSON number not supported", chars.location) // TODO support numbers

  private def nextCharOrError()(using boundary.Label[Token]): Char =
    if chars.atPartEnd then boundary.break(Token.Error("unexpected end", chars.location))
    else chars.nextChar()

  private def accept(char: Char, token: Token)(using boundary.Label[Token]): Token =
    nextCharOrError() match
      case `char` => token
      case next =>
        boundary.break(Token.Error(s"unexpected character: got $char but got $next", chars.location))

  private def accept(str: String, token: Token)(using boundary.Label[Token]): Token =
    for char <- str if char != nextCharOrError() do //
      boundary.break(Token.Error(s"expected `$char` (of $str)", chars.location))

    def isEndOfToken(char: Char): Boolean =
      char.isWhitespace || char == '}' || char == ']' || char == ','

    if !chars.atPartEnd && !isEndOfToken(chars.peekChar()) then
      boundary.break(Token.Error("expected end of token", chars.location))
    else
      token

    // TODO def error(...)(using boundary.Label[Token])
