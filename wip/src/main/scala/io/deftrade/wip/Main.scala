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
  * Do: all the things.
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
  CartesianStuff        |> discardValue
  TraverseStuff         |> discardValue

  /*
   * OK do moar things.
   */
  PureConfigExample |> discardValue
  SpireExamples     |> discardValue
}
