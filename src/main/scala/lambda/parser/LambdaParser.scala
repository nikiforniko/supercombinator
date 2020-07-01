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
    (LetRecToken() ~> rep1sep((varP <~ Assign()) ~ term, Comma()) <~ In()) ~ term ^^ {
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
    (SUM() | SUB() | MUL() | DIV() | GE() | GT() | LE() | LT() | EQ() | NE() | IF()) ^^ {
      case SUM() => IntSum()
      case SUB() => IntSub()
      case MUL() => IntMult()
      case DIV() => IntDiv()
      case GE()  => IntGe()
      case GT()  => IntGt()
      case LE()  => IntLe()
      case LT()  => IntLt()
      case EQ()  => IntEq()
      case NE()  => IntNe()
      case IF()  => IFClause()
    }
  }
  lazy val termWOAppl
      : PackratParser[Term] = letRecP | letP | abstrP | builtin | number | bool | varP | (LeftBracket() ~> term <~ RightBracket())

  lazy val term: PackratParser[Term] = positioned {
    rep1(termWOAppl) ^^ {
      case x :: xs =>
        xs.foldLeft(x) { (func, arg) =>
          Appl(func, arg)
        }
    } | failure("illegal start of term")
  }

  def Parse(tokens: Seq[LambdaToken]): Either[String, Term] =
    term.apply(new PackratReader(new LambdaTokenReader(tokens))) match {
      case Success(result: Term, next) => {
        println(result)
        Right(result)
      }
      case NoSuccess(msg, next) =>
        Left(s"$msg, at ${next.pos.line}:${next.pos.column}")
    }
}
