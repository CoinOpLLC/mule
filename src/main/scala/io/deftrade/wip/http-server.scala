/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package wip

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
    path("/yerf" / Segment / IntNumber) { (seg, i) =>
      get {
        complete(s"""$seg Yerf! ${"😼" * i}""")
      }
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
  Http().bindAndHandle(routes, "0.0.0.0", 8888) |> discardValue
}
