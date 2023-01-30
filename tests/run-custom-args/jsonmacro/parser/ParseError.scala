package jsonlib.parser

private[jsonlib] final case class ParseError(msg: String, part: Int, offset: Int)
