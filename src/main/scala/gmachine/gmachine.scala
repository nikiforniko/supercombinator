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

  val service = CORS(HttpService {
    case req@POST -> Root => {
      req.as(jsonOf[Input]) flatMap (
        input => {
          InstructionsParser.ParseAll(input.code) match {
            case Left(err) => BadRequest(err)
            case Right(s) => Ok(MySystem.run(s).asJson)
          }
        })
      }
  }, CORSConfig(
      anyOrigin = true,
      anyMethod = true,
      allowCredentials = true,
      maxAge = 1.day.toSeconds)
    )

  def server(args: List[String]) = BlazeBuilder.bindHttp(8080)
    .mountService(service, "/")
    .start
}
