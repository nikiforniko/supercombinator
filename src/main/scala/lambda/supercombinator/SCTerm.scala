package lambda.supercombinator

trait SCTerm

case class SCVar(name: String) extends SCTerm {
  override def toString: String = name.toString
}

case class SCDef(vars: List[SCVar], body: SCTerm) extends SCTerm {
  override def toString: String = "[" + vars.mkString(" ") + "]" + body
}

case class SCAppl(term1: SCTerm, term2: SCTerm) extends SCTerm {
  override def toString: String = s"($term1 $term2)"
}
