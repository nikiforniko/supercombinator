package lambda


case class SuperCombinatorDefinition(name: String, value: Abstr) {
  override def toString(): String = {
    def abstrStr(a: Abstr):String = {
      
      val other = a.body match {
        case b: Abstr => abstrStr(b)
        case default => a.body.toString()
      }
      a.variable + " " + other
    }
    "$"+name+" "+abstrStr(value)
  }
}

case class SuperCombinator(name: String) extends Term {
  override def toString = "$"+name
}

object SuperCombinator {
  var counter = 0
  def LambdaLifting(term: Term): Term = {
    val (newTerm, _, _) = lambdaLifting(term, Set[Var](), Set[Var](), false)
    newTerm
  }
  def lambdaLifting(term: Term, boundVariables: Set[Var], freeVariables: Set[Var], fromLambda: Boolean): (Term, Set[Var], Set[Var]) = {
    term match {
      case Abstr(variable, body) => {
        val (newTerm, newBound, newFree) = 
          if (fromLambda) {
            lambdaLifting(body, boundVariables + (variable), freeVariables, true)
          } else {
            lambdaLifting(body, Set(variable), freeVariables ++ boundVariables, true)
          }
        val newNewBound = newBound - variable
        if (newNewBound isEmpty) {
          (WrapAbstraction(Abstr(variable, newTerm), newFree), freeVariables, freeVariables.empty)
        } else {
          (newTerm, newNewBound, newFree)
        }
      }
      case Appl(first, second) => {
        val (firstTerm, firstBound, firstFree) = lambdaLifting(first, boundVariables, freeVariables, false)
        val (secondTerm, secondBound, secondFree) = lambdaLifting(second, boundVariables, freeVariables, false)
        (Appl(firstTerm, secondTerm), firstBound ++ secondBound, firstFree ++ secondFree)
      }
      case v: Var => {
        if (boundVariables contains v) {
          (v, boundVariables, freeVariables)
        } else {
          (v, boundVariables, freeVariables + v)
        }
      }
      case default => (term, boundVariables, freeVariables)
    }
  }
  def WrapAbstraction(abstr: Abstr, set: Set[Var]): Term = {
    if (set isEmpty) {
      counter = counter + 1
      println(SuperCombinatorDefinition(counter.toString, abstr))
      SuperCombinator(counter.toString)
    } else {
      val variable = set.head
      Appl(WrapAbstraction(Abstr(variable, abstr), set - variable), variable)
    }
  }
}
