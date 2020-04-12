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

import cats._, implicits._

/**
  * Do all the things.
  */
object Main extends App {

  /*
   *I wrote a little kata it goes like this...
   */
  val xs = List(1, 2, 3)
  // val oxs = List(1.some, None, 2.some, 3.some, None)
  val oxs = List(1.some, 2.some, 3.some)

  val izSaem = (xs zip oxs) forall {
    case (r, Some(l)) => r === l
    case _            => ???
  }

  izSaem |> assert

  /*
   * [cracks knuckles] OK now do all the (FP) things.
   */
  TreeStuff             |> discardValue
  StateMonadStuff       |> discardValue
  MonadTransformerStuff |> discardValue
  SemigroupalStuff      |> discardValue
  TraverseStuff         |> discardValue

  /*
   * OK do moar things.
   */
  PureConfigExample |> discardValue
  SpireExamples     |> discardValue
}

object FansiCrap {
  import fansi.Color.{ LightMagenta }
  def colorme[S <: AnyRef](s: S): String = (fansi.Str(s.toString) overlay LightMagenta).render
  def fade(n: Int) =
    (
      (0 to 255) map { i =>
        fansi.Back.True(i, 255 - i, 255)(" ")
      } grouped n map (_.mkString)
    ) mkString "\n"
}

// /**
//   * WIP - for otc derivative market participants (like hedge funds).
//   */
// trait Exotics {
//
//   /** A product only used for calibration. FIXME WTF */
//   case class Calibration()
//
//   /** Credit Default Swap */
//   case class Cds()
//
//   /** */
//   case class CdsIndex()
//
//   /** Constant Maturity Swap */
//   case class Cms()
//
//   /**
//     * [[https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf Deliverable Swap Forward]]
//     */
//   case class Dsf()
//
//   /** Forward Rate Agreement */
//   case class Fra()
//
//   /** A representation based on sensitivities. FIXME WTF */
//   case class Sensitivities()
//
//   /** */
//   case class Swap()
//
//   /** */
//   case class Swaption()
// }
//
// /**
//   * WIP - for fx derivative market participants (like banks).
//   */
// trait Fx extends {
//
//   /** FX Non-Deliverable Forward */
//   case class FxNdf()
//
//   /** */
//   case class FxSingle()
//
//   /** */
//   case class FxSingleBarrierOption()
//
//   /** */
//   case class FxSwap()
//
//   /** */
//   case class FxVanillaOption()
// }
//
// /**
//   * WIP - for otc derivative market participants (like banks).
//   */
// trait Ibor extends {
//
//   /** */
//   case class IborCapFloor()
//
//   /** */
//   case class IborFuture()
//
//   /** */
//   case class IborFutureOption()
// }

// /** monadic but just a toy implementation */
// private sealed abstract case class Lazy[A] private (thunk: Lazy.Thunk[A]) {
//   import Lazy.{ defer, Memo }
//   final def map[B](f: A => B): Lazy[B]           = new Lazy(Memo(() => f(thunk()))) {}
//   final def flatMap[B](f: A => Lazy[B]): Lazy[B] = defer(f(thunk()))
//   final def value: A                             = thunk()
// }
//
// /** implements [[cats.Defer]]`[Lazy]` */
// private object Lazy { outer =>
//
//   import cats.Defer
//
//   type Thunk[A] = () => A
//   final case class Memo[A](thunk: Thunk[A]) extends Thunk[A] {
//     lazy val memo: A   = thunk()
//     override def apply = memo
//   }
//
//   def later[A](a: => A): Lazy[A] = new Lazy(Memo(() => a)) {}
//
//   def defer[A](fa: => Lazy[A]) = later(fa.thunk())
//
//   lazy val lazyDefer: Defer[Lazy] = new Defer[Lazy] {
//     def defer[A](fa: => Lazy[A]) = outer defer fa
//   }
// }

// type LzCon = Lazy[Contract]
// import Lazy.later
