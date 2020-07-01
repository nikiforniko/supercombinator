package lambda.compiler

import gmachine.parser._
import lambda.supercombinator._
import lambda.parser._

object Compiler {

  // TODO(a.eremeev) split buildFunctions and Build MAIN
  def BuiltIn2ArgDef(i: Instruction): List[Instruction] = {
    Push(1) :: Eval :: Push(1) :: Eval :: i :: Update(3) :: Pop(2) :: Unwind :: Nil
  }
  var labelCounter: Int = 0;
  var counter: Int = 0;
  var Funcs =
    scala.collection.mutable.HashMap.empty[String, (Int, List[Instruction])];
  def CScheme(
      prog: SCTerm,
      n: Int,
      r: Map[SCVar, Int],
      names: Map[String, String]
  ): List[Instruction] = {
    prog match {
      case IFClause() => {
        labelCounter += 2
        val code = Push(0) :: Eval :: JFalse(labelCounter - 1) :: Push(1) :: Jump(
          labelCounter
        ) :: Label(labelCounter - 1) :: Push(2) :: Label(labelCounter) :: Eval :: Update(
          4
        ) :: Pop(3) :: Unwind :: Nil
        val name = "IF" + (labelCounter - 1).toString + labelCounter.toString
        Funcs += (name -> (3, code))
        PushGlobal(name) :: Nil
      }
      case IntTerm(v)  => PushInt(v) :: Nil
      case BoolTerm(v) => PushBool(v) :: Nil
      case v: SCVar    => Push(n - (r get v get)) :: Nil // n - r(x)
      case SCAppl(fst, snd) =>
        CScheme(snd, n, r, names) ++ CScheme(fst, n + 1, r, names) :+ MkAp
      case d: SCDef => {
        val name = names.get(d.name) match {
          case None => d.name
          case Some(_) => {
            counter += 1
            d.name + counter.toString
          }
        }
        Funcs += (name -> (d.vars.length, FScheme(d, n, r)))
        List[Instruction](PushGlobal(name))
      }
      // Very risky fallback
      case SCFuncCall(name) => PushGlobal(names.getOrElse(name, name)) :: Nil
      case SCLet(v, t, in) =>
        CScheme(t, n, r, names) ++ CScheme(in, n + 1, r + (v -> (n + 1)), names) :+ Slide(
          1
        )
      case SCLetRec(assigns, in) => {
        // TODO(a.eremeev) val ... = assigns.length
        val newR = r ++ (assigns
          .map(_._1)
          .zip(Range(n + 1, n + 1 + assigns.length)))
        val newN = n + assigns.length
        Alloc(assigns.length) :: assigns
          .map(x => CScheme(x._2, newN, newR, names))
          .zip(Range(0, assigns.length))
          .map({
            case (l, k) => l :+ Update(assigns.length - k)
          })
          .flatten ++ CScheme(in, newN, newR, names) :+ Slide(assigns.length)
      }
      case _ => List[Instruction](PushGlobal(prog.toString))
    }
  }
  def RScheme(prog: SCTerm, n: Int, r: Map[SCVar, Int]): List[Instruction] = {
    CScheme(prog, n, r, Map.empty) :+ Update(n + 1) :+ Pop(n) :+ Unwind
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
      case IntGe()   => Ge
      case IntGt()   => Gt
      case IntLe()   => Le
      case IntLt()   => Lt
      case IntEq()   => Eq
      case IntNe()   => Ne
    }
  }
  def Compile(prog: SCTerm): List[Instruction] = {
    Funcs =
      scala.collection.mutable.HashMap.empty[String, (Int, List[Instruction])];
    counter = 0
    labelCounter = 0
    Funcs ++= List[BuiltIn](
      IntSum(),
      IntSub(),
      IntDiv(),
      IntMult(),
      IntGt(),
      IntGe(),
      IntLe(),
      IntLt(),
      IntEq(),
      IntNe()
    ).map(f => (f.toString -> ((2, BuiltIn2ArgDef(mapp(f))))))
    Funcs += ("MAIN" -> (0, RScheme(prog, 0, Map.empty)))
    Begin :: PushGlobal("MAIN") :: Eval :: End :: Funcs
      .map({ case (k, (n, b)) => GlobStart(k, n) :: b })
      .toList
      .flatten
  }
}
