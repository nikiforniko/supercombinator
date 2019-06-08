import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
object FormulaParser extends RegexParsers with PackratParsers {

    def varP: PackratParser[Var] = "[a-z]".r ^^ Var

    lazy val applP: PackratParser[Appl] = ("""\s*\(\s*""".r ~> term <~ " *".r) ~ (term <~ """\s*\)\s*""".r) ^^ {case x ~ y => Appl(x, y)}

    lazy val abstrP: PackratParser[Abstr] = ("λ" ~> varP <~ ".") ~ term ^^ {case v ~ b => Abstr(v, b)}
    def number: Parser[IntTerm] = "[0-9]+".r ^^ (s => IntTerm(s.toInt))
    
    def builtInFunc: Parser[BuiltIn] = """[-+*/]""".r ^^ ({
      case "+" => IntSum
      case "-" => IntSub
      case "*" => IntMult
      case "/" => IntDiv
    })
    lazy val term: PackratParser[Term] = varP | applP | abstrP | builtInFunc | number

    def apply(code: String): Option[Term] =
      parse(term, new PackratReader(new CharSequenceReader(code))) match {
        case Success(result: Term, next) => Some(result)
        case NoSuccess(msg, next) =>{
          println(msg)
          None
        }
      }
    
}
