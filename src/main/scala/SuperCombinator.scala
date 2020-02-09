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

object SuperCombinator {
  def LambdaLifting(term: Term): SCTerm = {
    val (newTerm, _, _) = lambdaLifting(term, Set[Var](), Set[Var](), false)
    newTerm
  }
  def lambdaLifting(
      term: Term,
      boundVariables: Set[Var],
      freeVariables: Set[Var],
      fromLambda: Boolean
  ): (SCTerm, Set[Var], Set[Var]) = {
    term match {
      case Abstr(variable, body) => {
        val (newSP, newBound, newFree) =
          if (fromLambda) {
            lambdaLifting(
              body,
              boundVariables + (variable),
              freeVariables,
              true
            )
          } else {
            lambdaLifting(body, Set(variable), freeVariables, true)
          }
        val func: SCDef = newSP match {
          case SCDef(vars, body) => SCDef(SCVar(variable name) +: vars, body)
          case default           => SCDef(List(SCVar(variable.name)), newSP)
        }
        val newNewBound = newBound - variable
        if (newNewBound isEmpty) {
          (
            ApplyNArgs(
              SCDef(
                newFree.toList.map(x => SCVar(x.name)) ++ func.vars,
                func.body
              ),
              newFree.toList.map(x => SCVar(x.name))
            ),
            freeVariables,
            newNewBound
          )
        } else {
          (func, newNewBound, newFree)
        }
      }
      case Appl(first, second) => {
        val (firstSP, firstBound, firstFree) =
          lambdaLifting(first, boundVariables, freeVariables, false)
        val (secondSP, secondBound, secondFree) =
          lambdaLifting(second, boundVariables, freeVariables, false)
        (
          SCAppl(firstSP, secondSP),
          firstBound ++ secondBound,
          firstFree ++ secondFree
        )
      }
      case v: Var => {
        if (boundVariables contains v) {
          (SCVar(v.name), boundVariables, freeVariables)
        } else {
          (SCVar(v.name), boundVariables, freeVariables + v)
        }
      }
      case b: BuiltIn => (b, boundVariables, freeVariables)
    }
  }
  def ApplyNArgs(sp: SCTerm, vars: List[SCVar]): SCTerm =
    vars.headOption.fold(sp)(x => ApplyNArgs(SCAppl(sp, x), vars.tail))
}
