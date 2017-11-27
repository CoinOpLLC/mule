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

package wip

import cats.data.EitherT
import cats.syntax.either._
import cats.instances.future._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MonadTransformerStuff {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    val pl = powerLevels get autobot match {
      case Some(p) => p.asRight[String]
      case None    => s"$autobot power level unknown.".asLeft[Int]
    }
    Future(pl) |> EitherT.apply
  }

  def getPowerLevelMuchBetter(autobot: String): Response[Int] =
    (powerLevels get autobot).fold[Response[Int]](
      EitherT left Future(s"$autobot power level unknown.")
    ) { pl =>
      EitherT right Future(pl)
    }

  def hrResponse = for (hr <- getPowerLevel("Hot Rod")) yield hr

  def canSpecialMove(
      ally1: String,
      ally2: String
  ): Response[Boolean] =
    for {
      a1 <- getPowerLevel(ally1)
      a2 <- getPowerLevel(ally2)
    } yield a1 + a2 > 15

  def tacticalReportOldAndTired(ally1: String, ally2: String): String = {
    val csm = Await.result(canSpecialMove(ally1, ally2).value, 1.second)
    csm match {
      case Right(true)  => s"$ally1 and $ally2 are ready to rock!"
      case Right(false) => s"$ally1 and $ally2 need refractory respite!"
      case Left(msg)    => s"WTF: $msg"
    }
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val fa = (msg: String) => s"WTF: $msg"
    val fb = (can: Boolean) =>
      can match {
        case true  => s"$ally1 and $ally2 are ready to rock!"
        case false => s"$ally1 and $ally2 need refractory respite!"
    }
    Await.result(
      (canSpecialMove(ally1, ally2).value map (_ fold (fa, fb))),
      1.second
    )
  }
}
