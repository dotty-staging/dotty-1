package jsonmacro

import scala.util.boundary

object Parser:
  enum Result:
    case Parsed(json: Json)
    case Error(msg: String, offset: Int)

private class Parser(source: String):
  import Parser.Result, Result.*

  private var offset = 0
  skipWhiteSpaces()

  def parse(): Result =
    parseValue() match
      case parsed: Parsed =>
        if !atEnd then Error("value ended", offset)
        else parsed
      case err: Error => err

  def atEnd: Boolean =
    offset >= source.length

  private def peek(): Char =
    source(offset)

  private def next() =
    assert(!atEnd)
    val char = source(offset)
    offset += 1
    skipWhiteSpaces()
    char

  private def skipWhiteSpaces() =
    while !atEnd && source(offset).isWhitespace do
      offset += 1

  private def parseValue(): Result =
    if atEnd then Error("expected value", offset)
    else peek() match
      case '{' => parseObject()
      case '[' => parseArray()
      case '"' => parseString()
      case 't' => parseTrue()
      case 'f' => parseFalse()
      case 'n' => parseNull()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  => parseNumber()
      case char => Error(s"expected JSON value", offset)

  private def parseObject(): Result =
    assert(next() == '{')

    boundary {
      while !atEnd do
        next() match
          case '}' => boundary.break(Parsed(Json.Obj(Map.empty)))
          case _ =>
      Error("object not closed", offset)
    }

  private def parseArray(): Result =
    assert(next() == '[')
    if !atEnd && peek() == ']' then
      next()
      Parsed(Json.Arr())
    else
      var values = Array.newBuilder[Json]
      parseValue() match
        case Parsed(json) => json
        case err: Error => return err
      boundary {
        while !atEnd do
          next() match
            case ']' => boundary.break(Parsed(Json.Arr(values.result()*)))
            case ',' =>
              parseValue() match
                case Parsed(value) =>
                  values += value
                case err: Error =>
                  boundary.break(err)
        Error("array not closed", offset)
      }

  private def parseString(): Result = ???
  private def parseNumber(): Result = ???

  private def parseTrue(): Result =
    assert(next() == 't')
    if !atEnd && next() == 'r'
    && !atEnd && next() == 'u'
    && !atEnd && next() == 'e' // TODO check boundary
    then Parsed(Json.Bool(true))
    else Error("expected `true`", offset)

  private def parseFalse(): Result =
    assert(next() == 'f')
    if !atEnd && next() == 'a'
    && !atEnd && next() == 'l'
    && !atEnd && next() == 's'
    && !atEnd && next() == 'e' // TODO check boundary
    then Parsed(Json.Bool(false))
    else Error("expected `false`", offset)

  private def parseNull(): Result =
    assert(next() == 'n')
    if !atEnd && next() == 'u'
    && !atEnd && next() == 'l'
    && !atEnd && next() == 'l' // TODO check boundary
    then Parsed(Json.Null)
    else Error("expected `null`", offset)

