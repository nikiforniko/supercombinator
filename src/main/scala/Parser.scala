import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object FormulaParser extends RegexParsers with PackratParsers {

  def varP: PackratParser[Var] = "[a-z]".r ^^ Var

  lazy val applP
      : PackratParser[Appl] = ("""\s*(\s*""".r ~> term <~ """\s+""".r) ~ (term <~ """\s*\)\s*""".r) ^^ {
    case x ~ y => Appl(x, y)
  }

  lazy val abstrP: PackratParser[Abstr] = ("\\" ~> varP <~ ".") ~ term ^^ {
    case v ~ b => Abstr(v, b)
  }

  def number: Parser[IntTerm] = "[0-9]+".r ^^ (s => IntTerm(s.toInt))

  def bool: Parser[BoolTerm] =
    "TRUE|FALSE".r ^^ ({
      case "TRUE"  => BoolTerm(true)
      case "FALSE" => BoolTerm(false)
    })

  def builtInFunc: Parser[BuiltIn] =
    """[-+*/]|>=|IF""".r ^^ ({
      case "+"  => IntSum
      case "-"  => IntSub
      case "*"  => IntMult
      case "/"  => IntDiv
      case ">=" => IntGte
      case "IF" => IFClause
    })
  lazy val term
      : PackratParser[Term] = abstrP | varP | applP | builtInFunc | number | bool

  def Parse(code: String): Either[String, Term] =
    parse(term, new PackratReader(new CharSequenceReader(code))) match {
      case Success(result: Term, next) => Right(result)
      case NoSuccess(msg, next)        => Left(msg)
    }
}
