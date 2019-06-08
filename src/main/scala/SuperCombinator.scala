trait SPTerm

case class SPVar(name: String) extends SPTerm {
    override def toString: String = name.toString
}

case class SPDef(vars: List[SPVar], body: SPTerm) extends Combinator {
  override def toString: String = "[" + vars.mkString(" ") + "]" + body
  override def result(x: List[SPTerm]): Option[SPTerm] = if (x.length == vars.length) {
    val ctn = (vars zip x).toMap
    lazy val substitution:SPTerm => SPTerm = {
      case v: SPVar => ctn get v getOrElse v
      case SPAppl(t1, t2) => SPAppl(substitution(t1), substitution(t2))
      case default => default
    }
    Some(substitution(body))
  } else {
    None
  }
}

case class SPAppl(term1: SPTerm, term2: SPTerm) extends SPTerm {
    override def toString: String = s"($term1 $term2)"
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
            lambdaLifting(body, Set(variable), freeVariables, true)
          }
        val func: SPDef = newSP match {
          case SPDef(vars, body) => SPDef(SPVar(variable name) +:  vars, body)
          case default => SPDef(List(SPVar(variable.name)), newSP)
        }
        val newNewBound = newBound - variable
        if (newNewBound isEmpty) {
          (ApplyNArgs(SPDef(newFree.toList.map(x => SPVar(x.name)) ++ func.vars, func.body), newFree.toList.map(x => SPVar(x.name))), freeVariables, newNewBound)
        } else {
          (func, newNewBound, newFree)
        }
      }
      case Appl(first, second) => {
        val (firstSP, firstBound, firstFree) = lambdaLifting(first, boundVariables, freeVariables, false)
        val (secondSP, secondBound, secondFree) = lambdaLifting(second, boundVariables, freeVariables, false)
        (SPAppl(firstSP, secondSP), firstBound ++ secondBound, firstFree ++ secondFree)
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
  def ApplyNArgs(sp: SPTerm, vars: List[SPVar]): SPTerm = vars.headOption.fold(sp)(x => ApplyNArgs(SPAppl(sp, x), vars.tail))
}

object ApplN {
  def unapply(term: SPTerm): Option[(Combinator, List[SPTerm])] = term match {
    case SPAppl(term1, term2) => term1 match {
      //case ApplN
        case _: SPAppl => unapply(term1).map(x => (x._1,  x._2 :+ term2))
        case c: Combinator => Some((c, List[SPTerm](term2)))
        case _ => None
      }
    case _ => None
  }
}

object SPReduce {
  val combApply: SPTerm => Option[SPTerm] = {
    case ApplN(applicable, paramsList) =>
      applicable.result(paramsList)
    case default => {
      None
    }
  }

  def mu(reduction: SPTerm => Option[SPTerm]): SPTerm => Option[SPTerm] = {
    case SPAppl(term, params) => reduction(term).map(SPAppl(_, params))
    case _ => None
  }

  def nu(reduction: SPTerm => Option[SPTerm]): SPTerm => Option[SPTerm] = {
    case SPAppl(term, param) => reduction(param).map(SPAppl(term, _))
    case _ => None
  }

  def combMuNu(term: SPTerm ): Option[SPTerm] = {
      //reduction(x).getOrElse((mu(reduction)(x).getOrElse(nu(reduction)(x))))
      val l = List[SPTerm => Option[SPTerm]](combApply, mu(combMuNu), nu(combMuNu))
      l.foldLeft[Option[SPTerm]]((None))((x, y) =>
        x match {
          case None => y(term)
          case default => default
        }
      )
    }

  def toNormalForm(step: SPTerm => Option[SPTerm]): SPTerm => SPTerm =
    (term: SPTerm) => step(term).fold(term)(toNormalForm(step))
}
