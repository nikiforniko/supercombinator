package lambda.lexer

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

object LambdaLexer extends RegexParsers {
  def varName: Parser[VarName] = positioned {
    "[a-zA-z][a-zA-Z0-9_]*".r ^^ { str =>
      VarName(str)
    }
  }
  def intValue: Parser[IntValue] = positioned {
    """[-+]?[1-9]\d*|0""".r ^^ { str =>
      IntValue(str.toInt)
    }
  }
  def boolValue: Parser[BoolValue] = positioned {
    "TRUE|FALSE".r ^^ { str =>
      BoolValue(str.toBoolean)
    }
  }
  def lambdaStart = positioned { "\\" ^^ (_ => LambdaStart()) }
  def leftBracket = positioned { "(" ^^ (_ => LeftBracket()) }
  def rightBracket = positioned { ")" ^^ (_ => RightBracket()) }
  def arrow = positioned { "->" ^^ (_ => Arrow()) }
  def let = positioned { "let" ^^ (_ => LetToken()) }
  def letRec = positioned { "letrec" ^^ (_ => LetRecToken()) }
  def assign = positioned { "=" ^^ (_ => Assign()) }
  def in = positioned { "in" ^^ (_ => In()) }
  def sum = positioned { "+" ^^ (_ => SUM()) }
  def sub = positioned { "-" ^^ (_ => SUB()) }
  def mul = positioned { "*" ^^ (_ => MUL()) }
  def div = positioned { "/" ^^ (_ => DIV()) }
  def gte = positioned { ">=" ^^ (_ => GTE()) }
  def lte = positioned { "<=" ^^ (_ => LTE()) }
  def lt = positioned { "<" ^^ (_ => LT()) }
  def gt = positioned { ">" ^^ (_ => GT()) }
  def _eq = positioned { "==" ^^ (_ => EQ()) }
  def _ne = positioned { "!=" ^^ (_ => NE()) }
  def _if = positioned { "IF" ^^ (_ => IF()) }
  def ops = sum | sub | mul | div
  def cmps = gte | lte | lt | gt | _ne | _eq
  def brackets = leftBracket | rightBracket

  def tokens: Parser[List[LambdaToken]] = {
    phrase(
      rep1(
        arrow | ops | cmps | brackets | lambdaStart | intValue | boolValue | _if | letRec | let | assign | in | varName
      )
    ) ^^ (tokens => tokens)
  }
  def Parse(code: String): Either[String, List[LambdaToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        Left(s"$msg, at ${next.pos.line}:${next.pos.column}")
      case Success(result, next) => Right(result)
    }
  }
}
