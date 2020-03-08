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

sealed trait LetExp extends Term {
  val assigns: List[(Var, Term)]
  val in: Term
  val name: String

  override def toString(): String =
    name + "  " + assigns
      .map(x => x._1 + " = " + x._2)
      .mkString("\n") + " in " + in
  def copy(a: List[(Var, Term)], in: Term): LetExp
}

case class Let(assigns: List[(Var, Term)], in: Term) extends LetExp{
  override val name = "let"
  override def copy(a: List[(Var, Term)], in: Term): LetExp = Let(a, in)
}

case class LetRec(assigns: List[(Var, Term)], in: Term) extends LetExp{
  override val name = "letrec"
  override def copy(a: List[(Var, Term)], in: Term): LetExp = LetRec(a, in)
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
