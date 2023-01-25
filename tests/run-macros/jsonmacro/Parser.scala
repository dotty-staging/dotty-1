package jsonmacro

import scala.util.boundary, boundary.Label

object Parser:
  enum Result:
    case Parsed(json: Json)
    case Error(msg: String, offset: Int)

private class Parser(source: String):
  import Parser.Result, Result.*

  private var offset = 0

  def parse(): Result =
    skipWhiteSpaces()
    boundary {
      val parsed = parseValue()
      if !atEnd then error("value ended")
      else Parsed(parsed)
    }

  private def atEnd: Boolean =
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

  private def parseValue()(using Label[Result]): Json =
    if atEnd then error("expected value")
    else peek() match
      case '{' => parseObject()
      case '[' => parseArray()
      case '"' => parseString()
      case 't' => parseTrue()
      case 'f' => parseFalse()
      case 'n' => parseNull()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  => parseNumber()
      case char => error(s"expected JSON value")

  private def parseObject()(using Label[Result]): Json =
    assert(next() == '{')
    boundary {
      while !atEnd do
        next() match
          case '}' => boundary.break(Json.Obj(Map.empty))
          case _ => // FIXME
      error("object not closed")
    }

  private def parseArray()(using Label[Result]): Json =
    assert(next() == '[')
    if !atEnd && peek() == ']' then
      next()
      Json.Arr()
    else
      var values = Array.newBuilder[Json]
      values += parseValue()
      boundary {
        while !atEnd do
          next() match
            case ']' => boundary.break(Json.Arr(values.result()*))
            case ',' => values += parseValue()
        error("array not closed")
      }

  private def parseString()(using Label[Result]): Json = ???
  private def parseNumber()(using Label[Result]): Json = ???

  private def parseTrue()(using Label[Result]): Json =
    assert(next() == 't')
    if !atEnd && next() == 'r'
    && !atEnd && next() == 'u'
    && !atEnd && next() == 'e' // TODO check boundary
    then Json.Bool(true)
    else error("expected `true`")

  private def parseFalse()(using Label[Result]): Json =
    assert(next() == 'f')
    if !atEnd && next() == 'a'
    && !atEnd && next() == 'l'
    && !atEnd && next() == 's'
    && !atEnd && next() == 'e' // TODO check boundary
    then Json.Bool(false)
    else error("expected `false`")

  private def parseNull()(using Label[Result]): Json =
    assert(next() == 'n')
    if !atEnd && next() == 'u'
    && !atEnd && next() == 'l'
    && !atEnd && next() == 'l' // TODO check boundary
    then Json.Null
    else error("expected `null`")

  private def error(msg: String)(using Label[Result]): Nothing =
    boundary.break(Error(msg, offset))
