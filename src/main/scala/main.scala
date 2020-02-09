import scala.util.parsing.combinator._
object Main extends App {
  override def main(args: Array[String]) {
    //"((\x.\y.((+ (\t.x x)) (\t.y y)) 3) 4)"
    val er = FormulaParser.Parse(scala.io.StdIn.readLine())
    er match {
      case Left(value) => {
        println("Lambda: " + value)
        val sp = SuperCombinator.LambdaLifting(value)
        println("Supercombinator term: " + sp)
        println("G-Code:")
        println(Compiler.Compile(sp).mkString("\n"))
      }
      case Right(msg) => println(msg)
    }
  }
}
