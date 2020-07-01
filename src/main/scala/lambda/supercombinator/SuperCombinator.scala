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
      fromLambda: Boolean = false,
      funcName: Option[Var] = None
  ): (SCTerm, Set[Var], Set[Var], Option[List[SCVar]]) = {
    term match {
      case Abstr(variable, body) => {
        val abstrBound = funcName.map(Set(variable, _)).getOrElse(Set(variable))
        val (newSP, newBound, newFree, _) =
          if (fromLambda) {
            lambdaLifting(
              body,
              boundVariables ++ abstrBound,
              freeVariables,
              true
            )
          } else {
            lambdaLifting(body, abstrBound, freeVariables, true)
          }
        val func: SCDef = newSP match {
          case SCDef(vars, body, _) => SCDef(SCVar(variable name) +: vars, body)
          case default              => SCDef(List(SCVar(variable.name)), newSP)
        }
        val newNewBound = newBound -- abstrBound
        if (newNewBound isEmpty) {
          val vars = newFree.toList.map(x => SCVar(x.name))
          (
            ApplyNArgs(
              funcName.map(x =>
                SCDef(
                  vars ++ func.vars,
                  func.body,
                  x.name
                  )).getOrElse(
                    SCDef(
                      vars ++ func.vars,
                      func.body
                      )
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
      case Let(v, t, in) => {
        val passBounds = boundVariables + v
        val (scIn, boundsIn, freeIn, _) =
          lambdaLifting(in, passBounds, freeVariables)
        val (scT, boundsT, freeT, forWrap) =
          lambdaLifting(t, passBounds, freeVariables)
        val m = forWrap
          .map(x => Map(v.name -> ApplyNArgs(SCFuncCall(v.name), x)))
          .getOrElse(Map.empty)
        (
          SCLet(SCVar(v.name), scT, changeVariables(scIn, m)),
          boundsIn ++ boundsT - v,
          freeIn ++ freeT,
          None
        )
      }
      case LetRec(assigns, in) => {
        val passBounds = boundVariables ++ assigns.map(_._1)
        val (scRes, newBounds, newFree, _) =
          lambdaLifting(in, passBounds, freeVariables)
        val result = assigns.foldLeft(
          (SCLetRec(Nil, scRes), newBounds, newFree, Map.empty[String, SCTerm])
        )((z, x) => {
          val (term, bounds, free, forWrap) =
            lambdaLifting(x._2, passBounds, freeVariables, false, Some(x._1))
          val newMap = forWrap
            .map(
              w => z._4 + (x._1.name -> ApplyNArgs(SCFuncCall(x._1.name), w))
            )
            .getOrElse(z._4)
          (
            z._1.copy(z._1.assigns :+ (SCVar(x._1.name), term), z._1.in),
            z._2 ++ bounds,
            z._3 ++ free,
            newMap
          )
        })
        val newNewBound = result._2 -- assigns.map(_._1)

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
      case SCDef(vs, body, name) =>
        SCDef(
          vs,
          changeVariables(body, m.filterKeys(!vs.map(_.name).contains(_))),
          name
        )
      case SCLet(v, t, in) => {
        val newMap = m.filterKeys(
          k => includeLetVars || k != v.name
        )
        SCLet(v, changeVariables(t, newMap), changeVariables(in, newMap))
      }
      case SCLetRec(assigns, in) => {
        val newMap = m.filterKeys(
          k => includeLetVars || !assigns.map(_._1.toString).contains(k)
        )
        SCLetRec(
          assigns.map(x => (x._1, changeVariables(x._2, newMap))),
          changeVariables(in, newMap)
        )
      }
      case SCVar(name) => m.getOrElse(name, sp)
      case _           => sp
    }
  }
}
