package lambda.supercombinator

import lambda.parser._

object SuperCombinator {
  def LambdaLifting(term: Term): SCTerm = {
    val (newTerm, _, _, _) = lambdaLifting(term, Set[Var](), Set[Var]())
    newTerm
  }
  def lambdaLifting(
      term: Term,
      boundVariables: Set[Var],
      freeVariables: Set[Var],
      fromLambda: Boolean = false
  ): (SCTerm, Set[Var], Set[Var], Option[List[SCVar]]) = {
    term match {
      case Abstr(variable, body) => {
        val (newSP, newBound, newFree, _) =
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
          val vars = newFree.toList.map(x => SCVar(x.name))
          (
            ApplyNArgs(
              SCDef(
                vars ++ func.vars,
                func.body
              ),
              vars
            ),
            freeVariables,
            newNewBound,
            Some(vars)
          )
        } else {
          (func, newNewBound, newFree, None)
        }
      }
      case Appl(first, second) => {
        val (firstSP, firstBound, firstFree, _) =
          lambdaLifting(first, boundVariables, freeVariables)
        val (secondSP, secondBound, secondFree, _) =
          lambdaLifting(second, boundVariables, freeVariables)
        (
          SCAppl(firstSP, secondSP),
          firstBound ++ secondBound,
          firstFree ++ secondFree,
          None
        )
      }
      case le : LetExp => {
        val passBounds = boundVariables ++ le.assigns.map(_._1)
        val (scRes, newBounds, newFree, _) =
          lambdaLifting(le.in, passBounds, freeVariables)
        val result = le.assigns.foldLeft[(SCLetExp, Set[Var], Set[Var], Map[String, SCTerm])](
          (le match {
            case Let(_,_) => SCLet(Nil, scRes)
            case LetRec(_,_) =>  SCLetRec(Nil, scRes)
          }
          , newBounds, newFree, Map.empty[String, SCTerm])
        )((z, x) => {
          val (term, bounds, free, forWrap) =
            lambdaLifting(x._2, passBounds, freeVariables)
          val newMap = forWrap
            .map(w => z._4 + (x._1.name -> ApplyNArgs(SCVar(x._1.name), w)))
            .getOrElse(z._4)
          (
            z._1.copy(z._1.assigns :+ (SCVar(x._1.name), term), z._1.in),
            z._2 ++ bounds,
            z._3 ++ free,
            newMap
          )
        })
        val newNewBound = result._2 -- le.assigns.map(_._1)

        (
          changeVariables(result._1, result._4, true),
          newNewBound,
          result._3,
          None
        )
      }
      case v: Var => {
        if (boundVariables contains v) {
          (SCVar(v.name), boundVariables, freeVariables, None)
        } else {
          (SCVar(v.name), boundVariables, freeVariables + v, None)
        }
      }
      case b: BuiltIn => (b, boundVariables, freeVariables, None)
    }
  }
  def ApplyNArgs(sp: SCTerm, vars: List[SCVar]): SCTerm =
    vars.headOption.fold(sp)(x => ApplyNArgs(SCAppl(sp, x), vars.tail))

  def changeVariables(
      sp: SCTerm,
      m: Map[String, SCTerm],
      includeLetVars: Boolean = false
  ): SCTerm = {
    sp match {
      case SCAppl(fst, snd) =>
        SCAppl(changeVariables(fst, m), changeVariables(snd, m))
      case SCDef(vs, body) =>
        SCDef(
          vs,
          changeVariables(body, m.filterKeys(!vs.map(_.name).contains(_)))
        )
      case le: SCLetExp => {
        val newMap = m.filterKeys(
          k => includeLetVars || !le.assigns.map(_._1.toString).contains(k)
        )
        le.copy(
          le.assigns.map(x => (x._1, changeVariables(x._2, newMap))),
          changeVariables(le.in, newMap)
        )
      }
      case SCVar(name) => m.getOrElse(name, sp)
      case _           => sp
    }
  }
}
