/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
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

package wut

import scala.language.higherKinds

import cats.{ Eq, Eval, Functor, Id, Monad, Monoid }

import cats.syntax.eq._
import cats.syntax.option._

import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._

import cats.instances.int._
import cats.instances.string._
import cats.instances.tuple._

import cats.instances.option._
import cats.instances.vector._

object StaetMoanad {

  import cats.data.State

  type W9nState = State[Int, String]

  val w9n: W9nState = State { id =>
    (id, s"The state $id is the enemy.")
  }

  val inspektDemo = State.inspect[Int, String](state => s"The state $state is your friend.")

  inspektDemo.run(13).value === ((13, s"The state ${12 + 1} is your friend.")) |> assert

  type CalcState[A] = State[List[Int], A]

  val SadAssIntLexer = """(\d+)""".r

  type Op = (Int, Int) => Int
  def opState(op: Op): CalcState[Int] = State[List[Int], Int] {
    case top :: bis :: rest =>
      val res = op(top, bis)
      //println(res :: rest)
      (res :: rest, res)
    case _ => ???
  }
  def valState(ds: String): CalcState[Int] = State[List[Int], Int] {
    case stack =>
      val value = ds.toInt
      //println(value :: stack)
      (value :: stack, value)
  }
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => opState(_ + _)
    case "*" => opState(_ * _)
    case ds  => valState(ds)
  }

  evalOne("42").runA(Nil).value === 42 |> assert

  def evalAll(syms: List[String]): CalcState[Int] =
    syms.foldLeft(0.pure[CalcState]) { (acc, head) =>
      acc flatMap { _ =>
        head |> evalOne
      }
    }

  val program = evalAll(List("1", "2", "+", "3", "*"))
  program.runA(Nil).value === 9 |> assert

  val notherProg = for {
    _   <- evalAll(List("1", "2", "+"))
    _   <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  notherProg.runA(Nil).value === 21 |> assert
}

object MonadTransformerStuff {
  import cats.data.EitherT
  import cats.syntax.either._
  import cats.instances.future._

  import scala.concurrent.{ Await, Future }
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

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

  def hrResponse =
    for {
      hr <- getPowerLevel("Hot Rod")
    } yield hr

  // val hrFuture = hrResponse.value

  // val hrpl = Await.result(hrFuture, 1.second)
  //
  // hrpl === 10.asRight[String] |> discardValue

  def canSpecialMove(
      ally1: String,
      ally2: String
  ): Response[Boolean] =
    for {
      a1 <- getPowerLevel(ally1)
      a2 <- getPowerLevel(ally2)
    } yield a1 + a2 > 15

  def tacticalReportOldAndTired( // #FIXME: don't Await, map.
                                ally1: String,
                                ally2: String): String = {
    val csm = Await.result(canSpecialMove(ally1, ally2).value, 1.second)
    csm match {
      case Right(true)  => s"$ally1 and $ally2 are ready to rock!"
      case Right(false) => s"$ally1 and $ally2 need refractory respite!"
      case Left(msg)    => s"WTF: $msg"
    }

  }

  def tacticalReport(
      ally1: String,
      ally2: String
  ): String = {
    // val fa = (msg: String) => s"WTF: $ally1 and $ally2 TR fails with: $msg"
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
