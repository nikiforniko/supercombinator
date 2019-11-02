trait SCTerm

case class SCVar(name: String) extends SCTerm {
  override def toString: String = name.toString
}

case class SCDef(vars: List[SCVar], body: SCTerm)
    extends Combinator
    with SCTerm {
  override def toString: String = "[" + vars.mkString(" ") + "]" + body
  override def result(x: List[SCTerm]): Option[SCTerm] =
    if (x.length == vars.length) {
      val ctn = (vars zip x).toMap
      lazy val substitution: SCTerm => SCTerm = {
        case v: SCVar       => ctn get v getOrElse v
        case SPAppl(t1, t2) => SPAppl(substitution(t1), substitution(t2))
        case default        => default
      }
      Some(substitution(body))
    } else {
      None
    }
}

case class SPAppl(term1: SCTerm, term2: SCTerm) extends SCTerm {
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
          SPAppl(firstSP, secondSP),
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
    vars.headOption.fold(sp)(x => ApplyNArgs(SPAppl(sp, x), vars.tail))
}

object ApplN {
  def unapply(term: SCTerm): Option[(Combinator, List[SCTerm])] = term match {
    case SPAppl(term1, term2) =>
      term1 match {
        //case ApplN
        case _: SPAppl     => unapply(term1).map(x => (x._1, x._2 :+ term2))
        case c: Combinator => Some((c, List[SCTerm](term2)))
        case _             => None
      }
    case _ => None
  }
}

object SPReduce {
  val combApply: SCTerm => Option[SCTerm] = {
    case ApplN(applicable, paramsList) =>
      applicable.result(paramsList)
    case default => {
      None
    }
  }

  def mu(reduction: SCTerm => Option[SCTerm]): SCTerm => Option[SCTerm] = {
    case SPAppl(term, params) => reduction(term).map(SPAppl(_, params))
    case _                    => None
  }

  def nu(reduction: SCTerm => Option[SCTerm]): SCTerm => Option[SCTerm] = {
    case SPAppl(term, param) => reduction(param).map(SPAppl(term, _))
    case _                   => None
  }

  def combMuNu(term: SCTerm): Option[SCTerm] = {
    //reduction(x).getOrElse((mu(reduction)(x).getOrElse(nu(reduction)(x))))
    val l =
      List[SCTerm => Option[SCTerm]](combApply, mu(combMuNu), nu(combMuNu))
    l.foldLeft[Option[SCTerm]]((None))(
      (x, y) =>
        x match {
          case None    => y(term)
          case default => default
        }
    )
  }

  def toNormalForm(step: SCTerm => Option[SCTerm]): SCTerm => SCTerm =
    (term: SCTerm) => step(term).fold(term)(toNormalForm(step))
}
