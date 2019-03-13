import lambda._

object Main extends App {
  override def main(args: Array[String]) {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val f = Var("f")
    //val tru = Abstr(x, Abstr(y, x))
    //val fls = Abstr(x, Abstr(y, y))
    val gav = Appl(Abstr(f, Appl(f, Appl(Abstr(x, Appl(f, x)), IntTerm(0)))), Abstr(x, Appl(Appl(IntMult, x), x)))
    println(gav)
    println(SuperCombinator.LambdaLifting(gav))
  }
}
