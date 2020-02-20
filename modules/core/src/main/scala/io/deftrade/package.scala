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

/**
  * Foundational toolkit for applications serving '''financial market participants'''.
  *
  * What financial market participants?
  *
  *   - Initial scope: small private equity funds, small hedge funds, family offices, RIAs,
  *   loan funds, real estate funds, ad-hoc seed venture funds, etc.
  *
  *   - Potential scope: banks, credit unions, CDFIs, broker/dealers, crypto exchanges,
  * and other actors with additional requirements.
  *
  * This toolkit provides the necessary types and methods to specify
  * `Abstract Algebraic Domain Model`s
  *   - `Abstract` because models are mapped to multiple materialized types
  * via ''natural transformations'' (i.e. multiple interpreters for the same language).
  *   - `Algebraic` because models are formally composable
  *   - `Domain Model`s because the abstract algebra aspires to legibility by
  * the afformentioned ''financial market participants''.
  */
package object deftrade extends deftrade.results.mixin {

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
