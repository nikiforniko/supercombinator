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

object Gmachine extends ServerApp {

  val service = CORS(
    HttpService {
      case req @ POST -> Root / "gcode" => {
        req.as(jsonOf[Input]) flatMap (input => {
          InstructionsParser.ParseAll(input.code) match {
            case Left(err) => BadRequest(err)
            case Right(s)  => Ok(MySystem.run(s).asJson)
          }
        })
      }
      case req @ POST -> Root / "lambda" => {
        req.as(jsonOf[Input]) flatMap (input =>
        FormulaParser.Parse(input.code) match {
          case Right(value) => {
            val sp = SuperCombinator.LambdaLifting(value)
            Ok(Output(Compiler.Compile(sp).map(_.toString)).asJson)
          }
          case Left(err) => BadRequest(err)
        })
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
