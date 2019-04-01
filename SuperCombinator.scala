package lambda

trait SPTerm

case class SPVar(name: String) extends SPTerm {
    override def toString():String = name.toString
}

case class SPDef(vars: Array[SPVar], body: SPTerm) extends SPTerm {
    override def toString():String = "λ" + vars.map(_.toString()).foldLeft("")(_ + " " + _) + "." + body.toString()
}

case class SPAppl(func: SPTerm, args: Array[SPTerm]) extends SPTerm {
    override def toString():String = "(" + func.toString() + " " + args.map(_ toString()).foldLeft("")(_ + " " + _) + ")"
}


object SuperCombinator {
  def LambdaLifting(term: Term): SPTerm = {
    val (newTerm, _, _) = lambdaLifting(term, Set[Var](), Set[Var](), false)
    newTerm
  }
  def lambdaLifting(term: Term, boundVariables: Set[Var], freeVariables: Set[Var], fromLambda: Boolean): (SPTerm, Set[Var], Set[Var]) = {
    term match {
      case Abstr(variable, body) => {
        val (newSP, newBound, newFree) = 
          if (fromLambda) {
            lambdaLifting(body, boundVariables + (variable), freeVariables, true)
          } else {
            lambdaLifting(body, Set(variable), freeVariables ++ boundVariables, true)
          }
        val func: SPDef = newSP match {
          case SPDef(vars, body) => SPDef(SPVar(variable name) +: vars, body)
          case default => SPDef(Array(SPVar(variable.name)), newSP)
        }
        val newNewBound = newBound - variable
        if (newNewBound isEmpty) {
          (SPAppl(SPDef(newFree.toArray.map(x => SPVar(x.name)) ++ func.vars, func.body), newFree.toArray.map(x => SPVar(x.name))), freeVariables, newNewBound)
        } else {
          (func, newNewBound, newFree)
        }
      }
      case Appl(first, second) => {
        val (firstSP, firstBound, firstFree) = lambdaLifting(first, boundVariables, freeVariables, false)
        val (secondSP, secondBound, secondFree) = lambdaLifting(second, boundVariables, freeVariables, false)
        (SPAppl(firstSP, Array(secondSP)), firstBound ++ secondBound, firstFree ++ secondFree)
      }
      case v: Var => {
        if (boundVariables contains v) {
          (SPVar(v.name), boundVariables, freeVariables)
        } else {
          (SPVar(v.name), boundVariables, freeVariables + v)
        }
      }
      case b:BuiltIn => (b, boundVariables, freeVariables)
    }
  }
}
