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
    "\\" + variable + " -> " + body
}

case class Let(v: Var, t: Term, in: Term) extends Term {
  override def toString(): String =
    s"let $v = $t in $in"
}

case class LetRec(assigns: List[(Var, Term)], in: Term) extends Term {
  override def toString(): String =
    "letrec  " + assigns
      .map(x => x._1 + " = " + x._2)
      .mkString("\n") + " in " + in
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

case class IntGe() extends BuiltIn {
  override def toString = "GE"
}

case class IntGt() extends BuiltIn {
  override def toString = "GT"
}

case class IntLe() extends BuiltIn {
  override def toString = "LE"
}

case class IntLt() extends BuiltIn {
  override def toString = "LT"
}

case class IntEq() extends BuiltIn {
  override def toString = "EQ"
}

case class IntNe() extends BuiltIn {
  override def toString = "NE"
}

case class IFClause() extends BuiltIn {
  override def toString = "IF"
}

case class BoolTerm(value: Boolean) extends BuiltIn {
  override def toString = value.toString.toUpperCase
}
