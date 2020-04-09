package gmachine.parser

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

case object Ge extends Instruction {
  override def toString = "GE"
}

case object Gt extends Instruction {
  override def toString = "GT"
}

case object Lt extends Instruction {
  override def toString = "LT"
}

case object Le extends Instruction {
  override def toString = "LE"
}

case object Eq extends Instruction {
  override def toString = "EQ"
}

case object Ne extends Instruction {
  override def toString = "NE"
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

case class Label(k: Int) extends Instruction {
  override def toString = s"LABEL $k"
}

case class Jump(k: Int) extends Instruction {
  override def toString = s"JUMP $k"
}

case class JFalse(k: Int) extends Instruction {
  override def toString = s"JFALSE $k"
}
