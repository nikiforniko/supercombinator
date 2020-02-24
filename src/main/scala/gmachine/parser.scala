import scala.util.parsing.combinator._
object InstructionsParser extends RegexParsers {
  val eol = System.getProperty("line.separator")
  val instructionMap: Map[String, Instruction] =
    (MkAp :: Get :: Eval :: Add :: Sub :: Div :: Mul :: Gte :: Begin :: End :: Ret :: Unwind :: Nil)
      .map(x => (x.toString, x))
      .toMap
  val simpleInstruction: Parser[Instruction] = instructionMap.keys
    .mkString("|")
    .r ^^ (instructionMap.get(_).get)
  val instructionWithInt
      : Parser[Instruction] = """(PUSHINT|PUSH|POP|SLIDE|UPDATE|ALLOC|LABEL|JUMP|JFALSE)""".r ~ """(\d+)""".r ^^ ({
    case "PUSH" ~ v    => Push(v.toInt)
    case "PUSHINT" ~ v => PushInt(v.toInt)
    case "SLIDE" ~ v   => Slide(v.toInt)
    case "UPDATE" ~ v  => Update(v.toInt)
    case "POP" ~ v     => Pop(v.toInt)
    case "ALLOC" ~ v   => Alloc(v.toInt)
    case "LABEL" ~ v   => Label(v.toInt)
    case "JUMP" ~ v   => Jump(v.toInt)
    case "JFALSE" ~ v   => JFalse(v.toInt)
  })
  val instructionWithString
      : Parser[Instruction] = "PUSHGLOBAL" ~ """(\w+)""".r ^^ ({
    case _ ~ v => PushGlobal(v)
  })
  val pushBool: Parser[Instruction] = "PUSHBOOL" ~ """(TRUE|FALSE)""".r ^^ ({
    case _ ~ "TRUE" => PushBool(true)
    case _ ~ "FALSE" => PushBool(false)
  })
  val globalStart
      : Parser[Instruction] = "GLOBSTART" ~ """(\w+)""".r ~ """(\d+)""".r ^^ ({
    case _ ~ s ~ k => GlobStart(s, k.toInt)
  })
  val instructions
      : Parser[Instruction] = instructionWithString | simpleInstruction | instructionWithInt | globalStart | pushBool

  def Parse(code: String): Either[String, Instruction] =
    parseAll(instructions, code) match {
      case Success(result: Instruction, _) => Right(result)
      case NoSuccess(msg, next)            => Left(msg)
    }
  def ParseAll(code: String): Either[String, List[Instruction]] =
    code
      .split(eol)
      .foldLeft[Either[String, List[Instruction]]](Right(Nil))(
        (z, x) => z.flatMap(k => Parse(x).map(k :+ _))
      )
      .left
      .map("Syntax error: " + _)
}
