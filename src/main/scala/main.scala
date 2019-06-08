import scala.util.parsing.combinator._
object Main extends App {
  override def main(args: Array[String]) {
    val sp = SuperCombinator.LambdaLifting(FormulaParser("(((λf.λx.λy.((+ (f x)) (f y)) λx.((* x) x)) 3) 4)").get)
    println(SPReduce.toNormalForm(SPReduce.combMuNu)(sp))
    println(Compiler.EScheme(sp, 0, Map.empty))
    println(Compiler.Funcs)
  }
}
