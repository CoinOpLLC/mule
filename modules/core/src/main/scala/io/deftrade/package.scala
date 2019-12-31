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

package io

import cats.implicits._

/**
  * All abstract everything only.
  */
package object deftrade extends deftrade.results {

  /** */
  type Label   = refinements.Label
  type IsLabel = refinements.IsLabel

  /**
    * Informs wart remover that the value is intentionally discarded.
    *
    * Useful for checking whether a thing compiles at all. Hard to miss on a code review.
    */
  val discardValue: Any => Unit = (_: Any) => ()

  /**
    * Bind a message to an assertion function.
    *
    * Handy for development. If you write trading algos, development is basically "forever".
    */
  def assertOrElse(message: String): Boolean => Unit = assert(_, message)
}

package deftrade {

  import money.Currency
  import model.capital.Instrument

  /**
    * The answer to this question: "in what we price things?"
    *
    * `Numéraire` is formal finance term which, contrary to what a naive anglophone might think,
    * signifies the ''denominator'' for contracts and transactions.
    */
  sealed trait Numéraire

  /**
    * There are two ways we can settle the bill...
    *
    * ... this (top level) is where we declare the policy decisions about what those ways are.
    */
  object Numéraire {

    /** non-sealed extension point */
    trait InCoin extends Numéraire

    /** */
    object InCoin {

      /** */
      def unapply(n: Numéraire): Option[InCoin] = n match {
        case Currency(c) => c.some
        case _           => none
      }
    }

    /** non-sealed extension point */
    trait InKind extends Numéraire

    /** */
    object InKind {

      /** */
      def unapply(n: Numéraire): Option[InKind] = n match {
        case instrument @ Instrument(_, _, _, _, _) => instrument.some
        case _                                      => none
      }
    }
  }
}
