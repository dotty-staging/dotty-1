package jsonlib.parser

import jsonlib.util.*

private[jsonlib] class Parser(source: Seq[String]):

  private val tokens = new Tokens(source)
  private var interpolationsIndex = -1

  private type ![T] = Result.Continuation[AST, ParseError] ?=> T

  def parse(): Result[AST, ParseError] =
    Result.withContinuation {
      val ast = parseValue()
      accept(Token.End)
      ast
    }

  private def parseValue(): ![AST] =
    tokens.next() match
      case Token.Null => AST.Null
      case Token.True => AST.Bool(true)
      case Token.False => AST.Bool(false)
      case Token.Str(value) => AST.Str(value)
      case Token.Num(value) => AST.Num(???)
      case Token.OpenBrace =>
        val nameValues =
          if tokens.peek() == Token.CloseBrace then Vector.empty
          else commaSeparate(parseNameValue)
        accept(Token.CloseBrace)
        nameValues.map(_._1).groupBy(x => x).filter(_._2.length > 1).foreach { x =>
          error(s"Duplicate name: ${x._1}")
        }
        AST.Obj(nameValues*)
      case Token.OpenBracket =>
        val values =
          if tokens.peek() == Token.CloseBracket then Vector.empty
          else commaSeparate(parseValue)
        accept(Token.CloseBracket)
        AST.Arr(values*)
      case Token.InterpolatedValue =>
        interpolationsIndex += 1
        AST.InterpolatedValue(interpolationsIndex)
      case token =>
        error(s"unexpected start of value: $token")

  private def commaSeparate[T](parseItem: () => ![T]): ![Vector[T]] =
    def parseNext(values: Vector[T]): Vector[T] =
      tokens.peek() match
        case Token.Comma =>
          accept(Token.Comma)
          parseNext(values :+ parseItem())
        case _ => values
    parseNext(Vector(parseItem()))

  private def parseNameValue(): ![(String, AST)] =
    tokens.next() match
      case Token.Str(value) =>
        accept(Token.Colon)
        value -> parseValue()
      case _ =>
        error("expected string literal")

  private def accept(token: Token): ![Unit] =
    val nextToken = tokens.next()
    if token != nextToken then error(s"expected token $token but got $nextToken")

  private def error(msg: String): ![Nothing] =
    Result.continuation.error(ParseError(msg, tokens.part, tokens.offset))
