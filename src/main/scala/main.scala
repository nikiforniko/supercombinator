import scala.util.parsing.combinator._
object Main extends App {
  override def main(args: Array[String]) {
    //"((λx.λy.((+ (λt.x x)) (λt.y y)) 3) 4)"
    val sp = SuperCombinator.LambdaLifting(FormulaParser.Parse(scala.io.StdIn.readLine()).left.get)
    println("Supercombinator term: " + sp)
    println("Result of reduction: "+ SPReduce.toNormalForm(SPReduce.combMuNu)(sp))
    println("G-Code:")
    println(Compiler.EScheme(sp, 0, Map.empty).mkString("\n"))
    println(Compiler.Funcs.map({case (k, v) => k + ":\n" + v.mkString("\n")}).mkString("\n"))
  }
}
