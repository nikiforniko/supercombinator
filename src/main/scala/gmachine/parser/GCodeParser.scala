package gmachine.parser

import fastparse._
import fastparse.NoWhitespace._
import fastparse.internal.Util

object GCodeParser {

  def simpleInstruction[_: P] = P(
    "MKAP".!.map(_ => MkAp) |
      "GET".!.map(_ => Get) |
      "EVAL".!.map(_ => Eval) |
      "ADD".!.map(_ => Add) |
      "SUB".!.map(_ => Sub) |
      "DIV".!.map(_ => Div) |
      "MUL".!.map(_ => Mul) |
      "GE".!.map(_ => Ge) |
      "GT".!.map(_ => Gt) |
      "LE".!.map(_ => Le) |
      "LT".!.map(_ => Lt) |
      "NE".!.map(_ => Ne) |
      "EQ".!.map(_ => Eq) |
      "RET".!.map(_ => Ret) |
      "UNWIND".!.map(_ => Unwind))

  def comment[_: P] = P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")

  def newline[_: P] = P("\n" | "\r\n" | "\r" | "\f")

  def whitespace[_: P] = P(" " | "\t" | newline)

  def whitespaceToken[_: P] = P(comment | whitespace.rep(1))

  def ws[_: P] = P(whitespaceToken.rep)

  def digit[_: P] = CharIn("0-9")

  def letter[_: P]: P[Unit] = CharIn("a-zA-Z")

  def stringprefix[_: P]: P[Unit] = P(
    "PUSHINT" | "PUSH" | "POP" | "SLIDE" | "UPDATE" | "ALLOC" | "LABEL" | "JUMP" | "JFALSE"
  )

  def instructionWithIntToken[_: P]: P[Instruction] = P(stringprefix.! ~ whitespaceToken ~ digit.rep(1).!).map {

    case ("PUSHINT", v) => PushInt(v.toInt)
    case ("PUSH", v) => Push(v.toInt)
    case ("PUSHINT", v) => PushInt(v.toInt)
    case ("SLIDE", v) => Slide(v.toInt)
    case ("UPDATE", v) => Update(v.toInt)
    case ("POP", v) => Pop(v.toInt)
    case ("ALLOC", v) => Alloc(v.toInt)
    case ("LABEL", v) => Label(v.toInt)
    case ("JUMP", v) => Jump(v.toInt)
    case ("JFALSE", v) => JFalse(v.toInt)

  }

  def pushBoolToken[_: P]: P[PushBool] = P("PUSHBOOL" ~ whitespaceToken ~ ("TRUE" | "FALSE").!).map {
    case "TRUE" => PushBool(true)
    case "FALSE" => PushBool(false)
  }

  def instructionWithStringToken[_: P]: P[PushGlobal] = P("PUSHGLOBAL" ~ whitespaceToken ~ letter.rep.!).map(PushGlobal)

  def globalStartToken[_: P]: P[GlobStart] = P("GLOBSTART" ~ whitespaceToken ~ letter.rep.! ~ digit.rep(1).!).map {
    case (s, k) => GlobStart(s, k.toInt)
  }

  def instructions[_: P]: P[Instruction] = P(instructionWithStringToken | instructionWithIntToken | pushBoolToken | simpleInstruction)

  def beginToken[_: P] = P("BEGIN").map(_ => Begin)

  def endToken[_: P] = P("END").map(_ => End)

  def mainFunc[_: P] = P(beginToken ~ ws ~ instructions.rep(1, sep = ws) ~ ws ~ endToken).map {
    case (b, inst, end) => b +: inst :+ end
  }

  def globalStartFunc[_: P] = P(globalStartToken ~ ws ~ instructions.rep(1, sep = ws) ~ ws).map {
    case (gl, inst) => gl +: inst
  }

  def gCodeToken[_: P]: P[Seq[Instruction]] = P(mainFunc | globalStartFunc)
  def gCode[_: P] = P(gCodeToken.rep(1, sep = ws))

  def parseGCode(code: String): Either[String, List[Instruction]] = parse(code, gCode(_)).fold(
    (stack, index, extra) =>
      Left(s"Failure! at index: $index found: ... ${Util.literalize(extra.input.slice(index, index + 15))} expected: ${extra.trace().label}"),
    (v, _) => Right(v.flatten.toList))

  val i =
    """GLOBSTART Func1
      |  /* gjfg */
      | PUSH 1
      | GET SLIDE 2
      | GLOBSTART Func2
      |   MKAP UPDATE 1
      | BEGIN
      | PUSHBOOL TRUE
      |   EVAL PUSHGLOBAL fdds
      | END""".stripMargin
}


