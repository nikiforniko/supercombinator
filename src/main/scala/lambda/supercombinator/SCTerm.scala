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

trait SCLetExp extends SCTerm {
  val assigns: List[(SCVar, SCTerm)]
  val in: SCTerm
  val name: String
  override def toString: String =
    name + " " + assigns
      .map(x => x._1 + " = " + x._2)
      .mkString("\n") + " in " + in
  def copy(a: List[(SCVar, SCTerm)], in: SCTerm): SCLetExp
}

case class SCLet(assigns: List[(SCVar, SCTerm)], in: SCTerm) extends SCLetExp {
  override val name = "let"
  override def copy(a: List[(SCVar, SCTerm)], in: SCTerm): SCLetExp = SCLet(a, in)
}

case class SCLetRec(assigns: List[(SCVar, SCTerm)], in: SCTerm) extends SCLetExp {
  override val name = "letrec"
  override def copy(a: List[(SCVar, SCTerm)], in: SCTerm): SCLetExp = SCLetRec(a, in)
}
