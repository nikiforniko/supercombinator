import org.http4s._
import org.http4s.dsl._
import org.http4s.server.ServerApp
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.middleware._
import org.http4s.circe._

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.concurrent.duration._
import lambda.lexer.LambdaLexer
import lambda.parser.LambdaParser
import gmachine.parser.InstructionsParser
import gmachine.machine.Machine
import lambda.supercombinator.SuperCombinator
import lambda.compiler.Compiler

object Main extends ServerApp {

  val service = CORS(
    HttpService {
      case req @ POST -> Root / "gcode" => {
        req.as(jsonOf[Input]) flatMap (input => {
          InstructionsParser.ParseAll(input.code) match {
            case Left(err) => BadRequest(Error(err).asJson)
            case Right(s)  => Ok(Machine.run(s).asJson)
          }
        })
      }
      case req @ POST -> Root / "lambda" => {
        req.as(jsonOf[Input]) flatMap (
            input =>
              LambdaLexer
                .Parse(input.code)
                .flatMap(
                  tokens =>
                    LambdaParser
                      .Parse(tokens)
                      .flatMap(value => {
                        val sp = SuperCombinator.LambdaLifting(value)
                        Right(
                          Output(Compiler.Compile(sp).map(_.toString)).asJson
                        )
                      })
                ) match {
                case Left(a)  => BadRequest(Error(a).asJson)
                case Right(b) => Ok(b)
              }
          )
      }
    },
    CORSConfig(
      anyOrigin = true,
      anyMethod = true,
      allowCredentials = true,
      maxAge = 1.day.toSeconds
    )
  )

  def server(args: List[String]) =
    BlazeBuilder
      .bindHttp(sys.env.getOrElse("PORT", "8080").toString().toInt)
      .mountService(service, "/")
      .start
}

object Main2 {
  def main(args: Array[String]): Unit = {
    val input = Iterator
        .continually(readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    val res = for {
      tokens <- LambdaLexer.Parse(input)
      ast <- LambdaParser.Parse(tokens)
      val sp = SuperCombinator.LambdaLifting(ast)
      val code = Compiler.Compile(sp)
    } yield Machine.runRes(code)
    println(res)
  }
}
