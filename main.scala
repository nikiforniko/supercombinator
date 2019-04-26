import lambda._

object Main extends App {
  override def main(args: Array[String]) {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val f = Var("f")
    val g = Var("g")
    val p = Var("p")
    val t = Var("t")
    //val tru = Abstr(x, Abstr(y, x))
    //val fls = Abstr(x, Abstr(y, y))
    //val gav =
    //  Appl(
    //    Abstr(f,
    //      Abstr(g,
    //        Appl(
    //          f, Abstr(x,
    //            Appl(Appl(f, Appl(g, x)), IntTerm(0))
    //          )
    //        )
    //      )
    //    ),
    //    Abstr(x, Appl(Appl(IntMult, x), x))
    //  )
    //println(gav)
    //println(SuperCombinator.LambdaLifting(gav))
    //val gav2 = Abstr(f, Abstr(g, Abstr(x, Appl(f, Appl(g, x)))))
    //println(gav2)
    //println(SuperCombinator.LambdaLifting(gav2))
    val tru = Abstr(x, Abstr(y, x))
    val fls = Abstr(x, Abstr(y, y))

    val pair = Abstr(x, Abstr(y, Abstr(z, Appl(Appl(z, x), y))))
    val fst = Abstr(p, Appl(p,  Abstr(x, Abstr(y, x))))
    val snd = Abstr(p, Appl(p,  Abstr(x, Abstr(y, y))))
    val max = 
      Abstr(t,
        Appl(
          Appl(
            Appl(
              Appl(
                IntGtE,
                Appl(fst, t)
              ),
              Appl(snd, t)
              ),
            Appl(fst, t),
            ),
          Appl(snd, t)
        )
      )
    //println(fst)
    //println(SuperCombinator.LambdaLifting(fst))
    //println(max)
    println(max)
    //println(SuperCombinator.LambdaLifting(max))
    val (func, ar) = ApplN.unapply(SuperCombinator.LambdaLifting(max.body)).get
    //println(func, ar.mkString(","))
    import SPReduce._
    val a = SuperCombinator.LambdaLifting(max)
    println(SPAppl(a, SuperCombinator.LambdaLifting(Abstr(f, Appl(Appl(f, IntTerm(7)),IntTerm(100))))))
    println(toNormalForm(combMuNu)(SPAppl(a, SuperCombinator.LambdaLifting(Abstr(f, Appl(Appl(f, IntTerm(7)),IntTerm(100)))))))
  }
}
