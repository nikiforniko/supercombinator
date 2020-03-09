package lambda.supercombinator

trait SCTerm

case class SCVar(name: String) extends SCTerm {
  override def toString: String = name
}

case class SCDef(vars: List[SCVar], body: SCTerm) extends SCTerm {
  override def toString: String = "[" + vars.mkString(" ") + "]" + body
}

case class SCAppl(term1: SCTerm, term2: SCTerm) extends SCTerm {
  override def toString: String = s"($term1 $term2)"
}

case class SCLetRec(assigns: List[(SCVar, SCTerm)], in: SCTerm) extends SCTerm {
  override def toString: String =
    "letrec  " + assigns
      .map(x => x._1 + " = " + x._2)
      .mkString("\n") + " in " + in
}

case class SCLet(v: SCVar, t: SCTerm, in: SCTerm) extends SCTerm {
  override def toString: String = 
    s"let $v = $t in $in"
}
