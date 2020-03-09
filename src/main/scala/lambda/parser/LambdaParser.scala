package lambda.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import lambda.lexer._

object LambdaParser extends PackratParsers {
  override type Elem = LambdaToken
  class LambdaTokenReader(tokens: Seq[LambdaToken])
      extends Reader[LambdaToken] {
    override def first: LambdaToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position =
      tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[LambdaToken] = new LambdaTokenReader(tokens.tail)
  }

  def varP: PackratParser[Var] = positioned {
    _varP ^^ {
      case VarName(v) => Var(v)
    }
  }
  private def _varP: Parser[VarName] = positioned {
    accept("identifier", { case id @ VarName(v) => id })
  }

  lazy val applP: PackratParser[Appl] = positioned {
    LeftBracket() ~> term ~ term <~ RightBracket() ^^ {
      case x ~ y => Appl(x, y)
    }
  }

  lazy val abstrP: PackratParser[Abstr] = positioned {
    (LambdaStart() ~> varP <~ Arrow()) ~ term ^^ {
      case v ~ b => Abstr(v, b)
    }
  }

  lazy val letP: PackratParser[Let] = positioned {
    (LetToken() ~> ((varP <~ Assign()) ~ term) <~ In()) ~ term ^^ {
      case x ~ y ~ in => Let(x, y, in)
    }
  }

  lazy val letRecP: PackratParser[LetRec] = positioned {
    (LetRecToken() ~> rep1((varP <~ Assign()) ~ term) <~ In()) ~ term ^^ {
      case list ~ in => LetRec(list.map({ case x ~ y => (x, y) }), in)
    }
  }

  def number: Parser[IntTerm] = positioned {
    _number ^^ {
      case IntValue(i) => IntTerm(i)
    }
  }
  private def _number: Parser[IntValue] = positioned {
    accept("int constant", { case id @ IntValue(v) => id })
  }

  def bool: Parser[BoolTerm] = positioned {
    _bool ^^ {
      case BoolValue(b) => BoolTerm(b)
    }
  }
  private def _bool: Parser[BoolValue] = positioned {
    accept("bool constant", { case id @ BoolValue(v) => id })
  }

  def builtin: Parser[BuiltIn] = positioned {
    (SUM() | SUB() | MUL() | DIV() | GTE() | IF()) ^^ {
      case SUM() => IntSum()
      case SUB() => IntSub()
      case MUL() => IntMult()
      case DIV() => IntDiv()
      case GTE() => IntGte()
      case IF()  => IFClause()
    }
  }
  lazy val term
      : PackratParser[Term] = letRecP | letP | abstrP | applP | builtin | number | bool | varP

  def Parse(tokens: Seq[LambdaToken]): Either[String, Term] =
    term.apply(new PackratReader(new LambdaTokenReader(tokens))) match {
      case Success(result: Term, next) => Right(result)
      case NoSuccess(msg, next) =>
        Left(s"$msg, at ${next.pos.line}:${next.pos.column}")
    }
}
