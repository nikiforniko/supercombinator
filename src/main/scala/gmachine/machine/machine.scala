package gmachine.machine

import gmachine.parser._
import scala.annotation.tailrec

case class Node(
    id: Int,
    v: NodeValue
) {
  lazy val toNodeView = v match {
    case NodeApp(l, r) => NodeView(id, v.toString(), Some(l), Some(r))
    case _             => NodeView(id, v.toString(), None, None)
  }
}

trait NodeValue

case class NodeInt(i: Int) extends NodeValue {
  override def toString = s"$i"
}

case class NodeBool(b: Boolean) extends NodeValue {
  override def toString = s"$b"
}

case class NodeApp(
    l: Int,
    r: Int
) extends NodeValue {
  override def toString = s"@"
}

case class NodeFun(
    name: String,
    k: Int,
    c: List[Instruction]
) extends NodeValue {
  override def toString = s"$name"
}

case object NodeHole extends NodeValue {
  override def toString = "_"
}

case class Machine(
    counter: Int,
    commandsLimit: Int,
    stack: List[Int],
    graph: Map[Int, Node],
    commands: List[Instruction],
    dump: List[(List[Int], List[Instruction])],
    funcs: Map[String, NodeFun]
) {
  def getID(k: Int): Node = graph.get(k).get
}

case object Machine {
  def run(instrs: List[Instruction]): DiffWithErr =
    check(instrs) match {
      case s @ Some(x) => DiffWithErr(Nil, s)
      case None =>
        startJob(Machine(0, 1000, Nil, Map.empty, instrs, Nil, Map.empty))._1
    }
  def runRes(instrs: List[Instruction]): String =
    startJob(Machine(0, 100000, Nil, Map.empty, instrs, Nil, Map.empty)) match {
      case (DiffWithErr(_, Some(str)), _) => "ERROR: " + str
      case (_, Some(n)) => n.toString()
    }

  def initDiff(m: Machine, oldM: Machine): Diff = {
    Diff(
      oldM.commands.head.toString,
      GraphDiff(Nil, Nil),
      GraphDiff(Nil, Nil),
      m.stack.map(id => m.getID(id)).map(_.toNodeView)
    )
  }

  // TODO(a.eremeev) Тут нужно проверить, что все BEGIN, END, GLOBALSTART расставлены правильно
  def check(instrs: List[Instruction]): Option[String] = None

  def startJob(m: Machine): (DiffWithErr, Option[NodeValue]) = {
    val main = m.commands.tail.takeWhile(_ != End)
    val funcs = m.commands.tail
      .dropWhile(_ != End)
      .drop(1)
      .foldLeft[List[(String, NodeFun)]](Nil)(
        (z, x) =>
          x match {
            case GlobStart(name, k) => z :+ (name, (NodeFun(name, k, Nil)))
            case _ =>
              z.take(z.length - 1) :+ z.last
                .copy(_2 = z.last._2.copy(c = z.last._2.c :+ x))
          }
      )
      .toMap
    Recursive(MyMonad.unit(m.copy(commands = main, funcs = funcs))).mo match {
      case (diffs, Left(str)) =>(DiffWithErr(diffs, Some(str)), None)
      case (diffs, Right(m))        =>(DiffWithErr(diffs, None), m.stack.headOption.map(m.getID(_).v))
    }
  }
  @tailrec def Recursive(m: MyMonad[Machine]): MyMonad[Machine] = {
    val newRes = m.flatMap(
        machine =>
          if (machine.commandsLimit > 0) {
            MyMonad
              .unit(machine.copy(commandsLimit = machine.commandsLimit - 1))
          } else {
            MyMonad[Machine](Nil, Left("commands limit exceeded"))
          }
      )
      .flatMap(
        machine =>
          machine.commands match {
            case Nil => MyMonad.unit(machine)
            case _ => {
              val f = machine.commands.head match {
                case Push(k)       => push(k)
                case PushInt(n)    => pushInt(n)
                case PushBool(n)   => pushBool(n)
                case PushGlobal(s) => pushGlob(s)
                case Pop(k)        => pop(k)
                case Slide(k)      => slide(k)
                case Update(k)     => update(k)
                case Alloc(k)      => alloc(k)
                case Add           => ensureTwoIntsReturnInt(_ + _)
                case Mul           => ensureTwoIntsReturnInt(_ * _)
                case Div           => ensureTwoIntsReturnInt(_ / _)
                case Sub           => ensureTwoIntsReturnInt(_ - _)
                case Gte           => ensureTwoIntsReturnBool(_ >= _)
                case Gt           => ensureTwoIntsReturnBool(_ > _)
                case Lte           => ensureTwoIntsReturnBool(_ <= _)
                case Lt           => ensureTwoIntsReturnBool(_ < _)
                case Ne           => ensureTwoIntsReturnBool(_ != _)
                case Eq           => ensureTwoIntsReturnBool(_ == _)
                case MkAp          => mkAp()
                case Eval          => eval()
                case Unwind        => unwind()
                case Ret           => ret()
                case Label(_)      => skipInstr()
                case Jump(k)       => jump(k)
                case JFalse(k)     => jFalse(k)
                case _             => ???
              }
              f(machine)
            }
          }
      )
      newRes match {
        case MyMonad((_, Left(_))) => newRes
        case MyMonad((_, Right(machine))) => if
        (machine.commands.length == 0) {
          newRes
        } else {
          Recursive(newRes)
        }
      }
  }
  def push(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < k + 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only " + m.stack.length + " values, want " + (k + 1)
        )
      )
    } else {
      val newM = m.copy(
        stack = m.stack.drop(k).head :: m.stack,
        commands = m.commands.drop(1)
      )
      MyMonad(initDiff(newM, m) :: Nil, Right(newM))
    }
  }
  def pushInt(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    {
      val n = Node(m.counter, NodeInt(k))
      val newM = m.copy(
        counter = m.counter + 1,
        stack = m.counter :: m.stack,
        graph = m.graph + (n.id -> n),
        commands = m.commands.drop(1)
      )
      val diff =
        initDiff(newM, m).copy(add = GraphDiff(n.toNodeView :: Nil, Nil))
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  def pushBool(k: Boolean): Machine => MyMonad[Machine] = { (m: Machine) =>
    {
      val n = Node(m.counter, NodeBool(k))
      val newM = m.copy(
        counter = m.counter + 1,
        stack = m.counter :: m.stack,
        graph = m.graph + (n.id -> n),
        commands = m.commands.drop(1)
      )
      val diff =
        initDiff(newM, m).copy(add = GraphDiff(n.toNodeView :: Nil, Nil))
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  def pushGlob(k: String): Machine => MyMonad[Machine] = { (m: Machine) =>
    {
      m.funcs
        .get(k)
        .map(Node(m.counter, _))
        .map((n: Node) => {
          val newM = m.copy(
            counter = m.counter + 1,
            stack = m.counter :: m.stack,
            graph = m.graph + (n.id -> n),
            commands = m.commands.drop(1)
          )
          val diff =
            initDiff(newM, m).copy(add = GraphDiff(n.toNodeView :: Nil, Nil))
          MyMonad(diff :: Nil, Right(newM))
        })
        .getOrElse(MyMonad(Nil, Left(s"Not Found Function $k")))
    }
  }
  def pop(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < k + 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only " + m.stack.length + " values, want " + k
        )
      )
    } else {
      val newM =
        m.copy(stack = m.stack.drop(k), commands = m.commands.drop(1))
      val diff = initDiff(newM, m)
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  def slide(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < k + 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want ${k + 1}"
        )
      )
    } else {
      val newM = m.copy(
        stack = m.stack.head :: m.stack.drop(k + 1),
        commands = m.commands.drop(1)
      )
      val diff = initDiff(newM, m)
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  //TODO UINT
  def update(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < k + 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want ${k + 1}"
        )
      )
    } else if (m.stack.length == 0) {
      MyMonad(Nil, Left("UPDATE 0 isn't permitted"))
    } else {
      val kid = m.stack.drop(k).head
      val newM = m.copy(
        stack = m.stack.tail,
        graph = m.graph + (kid -> m.getID(m.stack.head).copy(id = kid)),
        commands = m.commands.drop(1)
      )
      val diff = initDiff(newM, m).copy(
        add = GraphDiff(newM.getID(kid).toNodeView :: Nil, Nil),
        remove = GraphDiff(m.getID(kid).toNodeView :: Nil, Nil)
      )
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  def alloc(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    {
      val n = Node(m.counter, NodeInt(k))
      val ids = (m.counter until (m.counter + k)).toList
        .foldLeft[List[Int]](Nil)((z, x) => x :: z)
      val mpairs = ids.map(x => x -> Node(x, NodeHole))
      val newM = m.copy(
        counter = m.counter + k,
        stack = ids ++ m.stack,
        graph = m.graph ++ mpairs.toMap,
        commands = m.commands.drop(1)
      )
      val diff = initDiff(newM, m).copy(
        add = GraphDiff(mpairs.unzip._2.map(_.toNodeView), Nil)
      )
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  def mkAp(): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < 2) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want 2"
        )
      )
    } else {
      val ap = Node(m.counter, NodeApp(m.stack.head, m.stack.tail.head))
      val newM = m.copy(
        stack = m.counter :: m.stack.drop(2),
        counter = m.counter + 1,
        graph = m.graph + (ap.id -> ap),
        commands = m.commands.drop(1)
      )
      val diff =
        initDiff(newM, m).copy(add = GraphDiff(ap.toNodeView :: Nil, Nil))
      MyMonad(diff :: Nil, Right(newM))
    }
  }
  // TODO(a.eremeev) make function that can check that there is enough values in stack
  def ensureTwoIntsReturnInt(
      f: (Int, Int) => Int
  ): Machine => MyMonad[Machine] = {
    ensureTwoInts((k1, k2, m) => {
      val newN = Node(m.counter, NodeInt(f(k1, k2)))
      val newM = m.copy(
        counter = m.counter + 1,
        stack = m.counter :: (m.stack.drop(2)),
        graph = m.graph + (m.counter -> newN),
        commands = m.commands.drop(1)
      )
      val diff = initDiff(newM, m).copy(
        add = GraphDiff(newN.toNodeView :: Nil, Nil)
      )
      MyMonad(diff :: Nil, Right(newM))
    })
  }

  def ensureTwoIntsReturnBool(
      f: (Int, Int) => Boolean
  ): Machine => MyMonad[Machine] = {
    ensureTwoInts((k1, k2, m) => {
      val n = Node(m.counter, NodeBool(f(k1, k2)))
      val newM = m.copy(
        counter = m.counter + 1,
        stack = m.counter :: (m.stack.drop(2)),
        graph = m.graph + (n.id -> n),
        commands = m.commands.drop(1)
      )
      val diff =
        initDiff(newM, m).copy(add = GraphDiff(n.toNodeView :: Nil, Nil))
      MyMonad(diff :: Nil, Right(newM))
    })
  }

  def ensureTwoInts(
      f: (Int, Int, Machine) => MyMonad[Machine]
  ): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < 2) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want 2"
        )
      )
    } else {
      m.stack.take(2).map(m.getID(_).v) match {
        case NodeInt(k1) :: NodeInt(k2) :: Nil => {
          f(k1, k2, m)
        }
        case _ => {
          val err = s"Bad Nodes on the top of stack want Int Nodes, Have ${m
            .getID(m.stack.head)
            .v} and ${m.getID(m.stack.tail.head).v}"
          MyMonad(Nil, Left(err))
        }
      }
    }
  }

  def eval(): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want 1"
        )
      )
    } else {
      val headN = m.getID(m.stack.head)
      val newM = headN.v match {
        case _: NodeApp =>
          m.copy(
            stack = m.stack.head :: Nil,
            commands = Unwind :: Nil,
            dump = (m.stack.tail, m.commands.tail) :: m.dump
          )
        case NodeFun(_, 0, coms) =>
          m.copy(
            stack = m.stack.head :: Nil,
            commands = coms,
            dump = (m.stack.tail, m.commands.tail) :: m.dump
          )
        case _ => m.copy(commands = m.commands.drop(1))
      }
      MyMonad(initDiff(newM, m) :: Nil, Right(newM))
    }
  }
  def unwind(): Machine => MyMonad[Machine] = { (m: Machine) =>
    if (m.stack.length < 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want 1"
        )
      )
    } else {
      val headN = m.getID(m.stack.head)
      val newM = headN.v match {
        case NodeApp(v1, v2) => m.copy(stack = v1 :: m.stack)
        case NodeFun(_, k, coms) => {
          if (m.stack.length + 1 < k) {
            m.copy(
              stack = m.stack.last :: m.dump.head._1,
              commands = m.dump.head._2,
              dump = m.dump.tail
            )
          } else {
            m.copy(
              stack = m.stack.tail
                .take(k)
                .map(m.getID(_).v)
                .map({ case NodeApp(_, n) => n }) ++ (m.stack.drop(k)),
              commands = coms
            )
          }
        }
        case _ =>
          m.copy(
            stack = m.stack.head :: (m.dump.head._1),
            commands = m.dump.head._2,
            dump = m.dump.tail
          )
      }
      MyMonad(initDiff(newM, m) :: Nil, Right(newM))
    }
  }
  def ret(): Machine => MyMonad[Machine] = { (m: Machine) =>
    // TODO(a.eremeev) check that enough values in commands
    if (m.stack.length < 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in stack for ${m.commands.head}, have only ${m.stack.length} values, want 1"
        )
      )
    } else if (m.dump.length < 1) {
      MyMonad(
        Nil,
        Left(
          s"not enough values in dump for ${m.commands.head}, have only ${m.dump.length} values, want 1"
        )
      )
    } else {
      val newM = m.copy(
        stack = m.stack.last :: m.dump.head._1,
        dump = m.dump.tail,
        commands = m.dump.head._2
      )
      MyMonad(initDiff(newM, m) :: Nil, Right(newM))
    }
  }

  def jump(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    {
      val newCommands = m.commands.dropWhile({
        case Label(m) => m != k
        case _        => true
      })
      newCommands match {
        case Nil => MyMonad(Nil, Left(s"can't found LABEL $k to JUMP"))
        case _ => {
          val newM = m.copy(
            commands = newCommands
          )
          MyMonad(initDiff(newM, m) :: Nil, Right(newM))
        }
      }
    }
  }

  def jFalse(k: Int): Machine => MyMonad[Machine] = { (m: Machine) =>
    {
      if (m.stack.length < 1) {
        MyMonad(
          Nil,
          Left(
            s"not enough values in stack for ${m.commands.head}, have only " + m.stack.length + " values, want " + 1
          )
        )
      } else {
        val hd = m.stack.head
        val newM = m.copy(stack = m.stack.tail)
        m.getID(hd).v match {
          case NodeBool(true) => {
            skipInstr()(newM)
          }
          case NodeBool(false) => jump(k)(newM)
          case _               => MyMonad(Nil, Left("not a bool node on the top of stack"))
        }
      }
    }
  }

  def skipInstr(): Machine => MyMonad[Machine] = { (m: Machine) =>
    val newM = m.copy(commands = m.commands.tail)
    MyMonad(initDiff(newM, m) :: Nil, Right(newM))
  }

}

case class MyMonad[T](mo: (List[Diff], Either[String, T])) {
  def flatMap[K](f: T => MyMonad[K]): MyMonad[K] = {
    MyMonad(mo match {
      case v @ (ls, Right(t)) =>
        f(t).mo match {
          case (l, Right(k)) => (ls ++ l, Right(k))
          case (l, left)     => (ls ++ l, left)
        }
      case (l, Left(mo)) => (l, Left(mo))
    })
  }
}

object MyMonad {
  def unit[T](t: T): MyMonad[T] = {
    MyMonad(Nil, Right(t))
  }
}
