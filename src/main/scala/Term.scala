trait Term

case class Var(name: String) extends Term {
  override def toString(): String = name
}

case class Appl(first: Term, second: Term) extends Term {
  override def toString(): String =
    "(" + first + " " + second + ")"
}

case class Abstr(variable: Var, body: Term) extends Term {
  override def toString(): String =
    "λ" + variable + "." + body + ""
}

trait BuiltIn extends Term with SCTerm
trait Combinator {
  def result(x: List[SCTerm]): Option[SCTerm]
}

case class IntTerm(value: Int) extends BuiltIn {
  override def toString = value.toString
}

case object IntSum extends BuiltIn with Combinator {
  override def toString = "sum"
  def result(x: List[SCTerm]): Option[SCTerm] = {
    val y = x
      .map({ case i: IntTerm => Some(i); case _ => None })
      .filter(!_.isEmpty)
      .map(_ get)
    if (x.length == 2 && y.length == 2) {
      Some(IntTerm(y.head.value + y.tail.head.value))
    } else {
      None
    }
  }
}

case object IntSub extends BuiltIn with Combinator {
  override def toString = "sub"
  def result(x: List[SCTerm]): Option[SCTerm] = {
    val y = x
      .map({ case i: IntTerm => Some(i); case _ => None })
      .filter(!_.isEmpty)
      .map(_ get)
    if (x.length == 2 && y.length == 2) {
      Some(IntTerm(y.head.value - y.tail.head.value))
    } else {
      None
    }
  }
}

case object IntDiv extends BuiltIn with Combinator {
  override def toString = "div"
  def result(x: List[SCTerm]): Option[SCTerm] = {
    val y = x
      .map({ case i: IntTerm => Some(i); case _ => None })
      .filter(!_.isEmpty)
      .map(_ get)
    if (x.length == 2 && y.length == 2) {
      Some(IntTerm(y.head.value / y.tail.head.value))
    } else {
      None
    }
  }
}

case object IntMult extends BuiltIn with Combinator {
  override def toString = "mul"
  def result(x: List[SCTerm]): Option[SCTerm] = {
    val y = x
      .map({ case i: IntTerm => Some(i); case _ => None })
      .filter(!_.isEmpty)
      .map(_ get)
    if (x.length == 2 && y.length == 2) {
      Some(IntTerm(y.head.value * y.tail.head.value))
    } else {
      None
    }
  }
}
