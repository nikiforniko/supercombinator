package lambda.compiler

import gmachine.parser._
import lambda.supercombinator._
import lambda.parser._

object Compiler {
  def BuiltIn2ArgDef(i: Instruction): List[Instruction] = {
    Push(1) :: Eval :: Push(1) :: Eval :: i :: Update(3) :: Pop(2) :: Unwind :: Nil
  }
  var counter: Int = 0;
  var labelCounter: Int = 0;
  var Funcs =
    scala.collection.mutable.HashMap.empty[String, (Int, List[Instruction])];
  def CScheme(prog: SCTerm, n: Int, r: Map[SCVar, Int], fName: Option[String]): List[Instruction] = {
    prog match {
      case SCAppl(SCAppl(SCAppl(IFClause(), cond), thn), els) => {
        labelCounter += 2
        (CScheme(cond, n, r) :+ Eval :+ JFalse(labelCounter - 1)) ++
          (CScheme(thn, n, r) :+ Jump(labelCounter) :+ Label(labelCounter - 1)) ++
          (CScheme(els, n, r) :+ Label(labelCounter))
      }
      case IntTerm(v)  => PushInt(v) :: Nil
      case BoolTerm(v) => PushBool(v) :: Nil
      case v: SCVar    => Push(n - (r get v get)) :: Nil // n - r(x)
      case SCAppl(fst, snd) =>
        CScheme(snd, n, r) ++ CScheme(fst, n + 1, r) :+ MkAp
      case d: SCDef => {
        counter += 1
        val funcName = s"f$counter"
        Funcs += (funcName -> (d.vars.length, FScheme(d, n, r)))
        List[Instruction](PushGlobal(funcName))
      }
      case SCLet(v, t, in) => CScheme(t, n, r) ++ CScheme(in, n + 1, r  + (v -> (n+1))) :+ Slide(1)
      case SCLetRec(assigns, in) => {
        // TODO(a.eremeev) val ... = assigns.length
        val newR = r ++ (assigns.map(_._1).zip(Range(n + 1, n + 1 +  assigns.length)))
        val newN = n + assigns.length
        Alloc(assigns.length) :: assigns.map(x => CScheme(x._2, newN, newR)).zip(Range(0, assigns.length)).map({
          case (l, k) => l :+ Update(assigns.length - k)
        }).flatten ++ CScheme(in, newN, newR) :+ Slide(assigns.length)
      }
      case _ => List[Instruction](PushGlobal(prog.toString))
    }
  }
  def RScheme(prog: SCTerm, n: Int, r: Map[SCVar, Int]): List[Instruction] = {
    CScheme(prog, n, r) :+ Update(n + 1) :+ Pop(n) :+ Unwind
  }
  def FScheme(func: SCDef, n: Int, r: Map[SCVar, Int]): List[Instruction] = {
    val newR = r ++ (func.vars.zip(Range(0, func.vars.length)) map ({
      case (v, i) => v -> (func.vars.length - i)
    }))
    RScheme(func.body, func.vars.length, newR)
  }
  def BScheme(prog: SCTerm, n: Int, r: Map[SCVar, Int]): List[Instruction] = {
    List[Instruction]()
  }
  def mapp(b: BuiltIn): Instruction = {
    b match {
      case IntSum()  => Add
      case IntSub()  => Sub
      case IntDiv()  => Div
      case IntMult() => Mul
      case IntGte()  => Gte
    }
  }
  def Compile(prog: SCTerm): List[Instruction] = {
    Funcs ++= List[BuiltIn](IntSum(), IntSub(), IntDiv(), IntMult(), IntGte())
      .map(f => (f.toString -> ((2, BuiltIn2ArgDef(mapp(f))))))
    Funcs += ("MAIN" -> (0, RScheme(prog, 0, Map.empty)))
    Begin :: PushGlobal("MAIN") :: Eval :: End :: Funcs
      .map({ case (k, (n, b)) => GlobStart(k, n) :: b })
      .toList
      .flatten
  }
}
