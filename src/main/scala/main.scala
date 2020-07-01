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
import gmachine.parser.GCodeParser
import gmachine.machine.Machine
import lambda.supercombinator.SuperCombinator
import lambda.compiler.Compiler

object Main extends ServerApp {

  val service = CORS(
    HttpService {
      case req @ POST -> Root / "gcode" => {
        req.as(jsonOf[InputGCode]) flatMap (input => {
          GCodeParser.parseGCode(input.code) match {
            case Left(err) => BadRequest(Error(err).asJson)
            case Right(s)  => Ok(Machine.run(s, input.onlyResult).asJson)
          }
        })
      }
      case req @ POST -> Root / "lambda" => {
        req.as(jsonOf[InputLambda]) flatMap (
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
      .bindHttp(sys.env.getOrElse("PORT", "8080").toString().toInt, "0.0.0.0")
      .mountService(service, "/")
      .start
}
