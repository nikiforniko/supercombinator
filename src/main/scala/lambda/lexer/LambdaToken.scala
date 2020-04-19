package lambda.lexer

import scala.util.parsing.input.Positional

sealed trait LambdaToken extends Positional

case class VarName(str: String) extends LambdaToken
case class IntValue(int: Int) extends LambdaToken
case class BoolValue(bool: Boolean) extends LambdaToken

case class Arrow() extends LambdaToken
case class LambdaStart() extends LambdaToken
case class LeftBracket() extends LambdaToken
case class RightBracket() extends LambdaToken
case class Comma() extends LambdaToken
case class LetToken() extends LambdaToken
case class LetRecToken() extends LambdaToken
case class Assign() extends LambdaToken
case class In() extends LambdaToken

case class SUM() extends LambdaToken
case class SUB() extends LambdaToken
case class MUL() extends LambdaToken
case class DIV() extends LambdaToken

case class GE() extends LambdaToken
case class LE() extends LambdaToken
case class LT() extends LambdaToken
case class GT() extends LambdaToken
case class EQ() extends LambdaToken
case class NE() extends LambdaToken

case class IF() extends LambdaToken
