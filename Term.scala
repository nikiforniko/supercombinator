package lambda

trait Term

case class Var(name: String) extends Term {
  override def toString(): String = name
}

case class Appl(first: Term, second: Term) extends Term {
  override def toString(): String =
    "("+first+" "+second+")"
}

case class Abstr(variable: Var, body: Term) extends Term {
  override def toString(): String =
    "λ"+variable+"."+body+""
}

trait BuiltIn extends Term with SPTerm

case class IntTerm(value: Int) extends BuiltIn {
  override def toString = value.toString
}

case object IntSum extends BuiltIn {
  override def toString = "+"
}

case object IntSub extends BuiltIn {
  override def toString = "-"
}

case object IntDiv extends BuiltIn {
  override def toString = "div"
}

case object IntMult extends BuiltIn {
  override def toString = "*"
}
