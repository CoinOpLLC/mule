package wut

import akka.http.{ scaladsl => hdsl }
import hdsl.Http
import hdsl.model.{ ContentTypes, HttpEntity, HttpResponse, ResponseEntity, StatusCodes }
import hdsl.server._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

object Server extends App with Directives {

  implicit val system                          = ActorSystem("actor-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val routes: Route =
    path("/yerf") {
      complete("Yerf! 😼")
    } ~ path("/yiff") {
      post {
        val resp: ResponseEntity = HttpEntity(
          ContentTypes.`application/json`,
          """{"key" : "value" }"""
        )
        complete(HttpResponse(StatusCodes.OK, entity = resp))
      }
    }
  //
  Http().bindAndHandle(routes, "0.0.0.0", 8002) |> discardValue

}
