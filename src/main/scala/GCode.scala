trait Instruction

case class Push(n: Int) extends Instruction {
  override def toString = s"PUSH $n"
}
case class PushFun(f: String) extends Instruction {
  override def toString = s"PUSHFUN $f"
}
case class PushInt(i: Int) extends Instruction {
  override def toString = s"PUSHINT $i"
}
case class Slide(n: Int) extends Instruction {
  override def toString = s"SLIDE $n"
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
case object Alloc extends Instruction {
  override def toString = "ALLOC"
}
case object Eval extends Instruction {
  override def toString = "EVAL"
}
case object Unwind extends Instruction {
  override def toString = "UNWIND"
}
case class Ret(n: Int) extends Instruction {
  override def toString = s"RET $n"
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

object Compiler {
  var counter: Int = 0;
  var Funcs = scala.collection.mutable.HashMap.empty[String,List[Instruction]];
  def CScheme(prog: SPTerm, n: Int, r: Map[SPVar,Int]): List[Instruction] = {
    prog match {
      case i: IntTerm    => List[Instruction](PushInt(i.value))
      case v: SPVar      => List[Instruction](Push(n - (r get v get))) // n - r(x)
      case SPAppl(fst, snd)     => CScheme(snd, n, r) ++ CScheme(fst, n+1, r) :+ MkAp
      case d: SPDef => {
        counter += 1
        val funcName = s"f$counter"
        Funcs += (funcName -> FScheme(d, n, r))
        List[Instruction](PushFun(funcName))
      }
      case _ =>  List[Instruction](PushFun(prog.toString))
    }
  }
  def EScheme(prog: SPTerm, n: Int, r: Map[SPVar,Int]): List[Instruction] = {
    CScheme(prog, n, r) :+ Eval
  }
  def FScheme(func: SPDef, n: Int, r: Map[SPVar,Int]): List[Instruction] = {
    val newR = r ++ (func.vars.zip(Range(0, func.vars.length)) map ({case (v, i) =>  v -> (2 * func.vars.length - i) }))
    EScheme(func.body, 2 * func.vars.length  + 1, newR)  :+ Update(2 * func.vars.length + 1) :+ Ret(2 * func.vars.length)
  }
  def BScheme(prog: SPTerm, n: Int, r: Map[SPVar,Int]): List[Instruction] = {
    List[Instruction]()
  }
}
