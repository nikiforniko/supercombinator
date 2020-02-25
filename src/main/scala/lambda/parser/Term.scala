package lambda.parser

import scala.util.parsing.input.Positional
import lambda.supercombinator.SCTerm

sealed trait Term extends Positional

case class Var(name: String) extends Term {
  override def toString(): String = name
}

case class Appl(first: Term, second: Term) extends Term {
  override def toString(): String =
    "(" + first + " " + second + ")"
}

case class Abstr(variable: Var, body: Term) extends Term {
  override def toString(): String =
    "λ" + variable + "->" + body + ""
}

trait BuiltIn extends Term with SCTerm

case class IntTerm(value: Int) extends BuiltIn {
  override def toString = value.toString
}

case class IntSum() extends BuiltIn {
  override def toString = "SUM"
}

case class IntSub() extends BuiltIn {
  override def toString = "SUB"
}

case class IntDiv() extends BuiltIn {
  override def toString = "DIV"
}

case class IntMult() extends BuiltIn {
  override def toString = "MUL"
}

case class IntGte() extends BuiltIn {
  override def toString = "GTE"
}

case class IFClause() extends BuiltIn {
  override def toString = "IF"
}

case class BoolTerm(value: Boolean) extends BuiltIn {
  override def toString = value.toString.toUpperCase
}
