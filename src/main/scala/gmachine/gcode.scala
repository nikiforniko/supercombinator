trait Instruction

case class Push(n: Int) extends Instruction {
  override def toString = s"PUSH $n"
}
case class Pop(n: Int) extends Instruction {
  override def toString = s"POP $n"
}
case class PushGlobal(f: String) extends Instruction {
  override def toString = s"PUSHGLOBAL $f"
}
case class PushInt(i: Int) extends Instruction {
  override def toString = s"PUSHINT $i"
}
case class PushBool(i: Boolean) extends Instruction {
  override def toString = s"PUSHBOOL $i".toUpperCase
}
case class Slide(n: Int) extends Instruction {
  override def toString = s"SLIDE $n"
}
case class Alloc(n: Int) extends Instruction {
  override def toString = s"ALLOC $n"
}
case class Update(n: Int) extends Instruction {
  override def toString = s"UPDATE $n"
}
case object Get extends Instruction {
  override def toString = "GET"
}
case object MkAp extends Instruction {
  override def toString = "MKAP"
}
case object MkInt extends Instruction {
  override def toString = "MKINT"
}
case object Eval extends Instruction {
  override def toString = "EVAL"
}
case object Unwind extends Instruction {
  override def toString = "UNWIND"
}
case object Ret extends Instruction {
  override def toString = "RETURN"
}

case object Add extends Instruction {
  override def toString = "ADD"
}

case object Sub extends Instruction {
  override def toString = "SUB"
}

case object Mul extends Instruction {
  override def toString = "MUL"
}

case object Div extends Instruction {
  override def toString = "DIV"
}

case object Gte extends Instruction {
  override def toString = "GTE"
}

case object Begin extends Instruction {
  override def toString = "BEGIN"
}
case object End extends Instruction {
  override def toString = "END"
}

case class GlobStart(s: String, k: Int) extends Instruction {
  override def toString = s"GLOBSTART $s $k"
}

case class Label(k: Int) extends Instruction{
  override def toString = s"LABEL $k"
}

case class Jump(k: Int) extends Instruction{
  override def toString = s"JUMP $k"
}

case class JFalse(k: Int) extends Instruction{
  override def toString = s"JFALSE $k"
}

object Compiler {
  def BuiltIn2ArgDef(i: Instruction): List[Instruction] = {
    Push(1) :: Eval :: Push(1) :: Eval :: i :: Update(3) :: Pop(2) :: Unwind :: Nil
  }
  var counter: Int = 0;
  var labelCounter: Int = 0;
  var Funcs =
    scala.collection.mutable.HashMap.empty[String, (Int, List[Instruction])];
  def CScheme(prog: SCTerm, n: Int, r: Map[SCVar, Int]): List[Instruction] = {
    prog match {
      case SCAppl(SCAppl(SCAppl(IFClause, cond), thn), els) => {
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
      case IntSum  => Add
      case IntSub  => Sub
      case IntDiv  => Div
      case IntMult => Mul
      case IntGte  => Gte
    }
  }
  def Compile(prog: SCTerm): List[Instruction] = {
    Funcs ++= List[BuiltIn](IntSum, IntSub, IntDiv, IntMult, IntGte)
      .map(f => (f.toString -> ((2, BuiltIn2ArgDef(mapp(f))))))
    Funcs += ("MAIN" -> (0, RScheme(prog, 0, Map.empty)))
    Begin :: PushGlobal("MAIN") :: Eval :: End :: Funcs
      .map({ case (k, (n, b)) => GlobStart(k, n) :: b })
      .toList
      .flatten
  }
}
