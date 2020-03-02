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

import cats.kernel.CommutativeGroup

/**
  * Single-import DSL for monetary expressions, including currency support.
  *
  *   ==Inspiration:==
  *   - [[https://www.joda.org/joda-money/ Joda Money]] of course
  *   - [[http://www.squants.com/ squants.market.Money]]
  *   - [[https://github.com/Appendium/objectlabkit ObjectLabKit]]
  *
  *   ==Implementation:==
  *   - leverages [[java.util.Currency]] support for
  *     [[https://en.wikipedia.org/wiki/ISO_4217 ISO 4217]]
  *       - incurs JVM dependency :(
  *   - [[Mny]] is a value class type constructor with two type parameters:
  *       - a numeric type `N` for which a `Financial[N]` instance is in implicit scope
  *           - entails [[cats.kernel.CommutativeGroup]] and [[spire.math.Fractional]]
  *       - a type `C` for which a [[Currency]]`[C]` instance is in implicit scope
  *           - `C` distinct for each supported currency
  *       - [[model.Money]][C] binds the `N` type in [[Mny]], leaving one parameter free
  *         (the currency `C`)
  *
  *   TODO: Integration with [[eu.timepit.refined.api.Refined]] via `RefType[F[_,_]]`
  *         was done, but abandonned due to lack
  *         of apparent utility and evident implicit scope ambiguity.
  *         Revisit if need be.
  *
  *   TODO: track evolving
  * [[https://en.wikipedia.org/wiki/ISO_4217#Cryptocurrencies crypto support]]
  *
  */
package object money {

  /** TODO: This will break down in corner cases! Review how other projects handle this. */
  implicit def financialCommutativeGroup[MA: Financial]: CommutativeGroup[MA] =
    Financial[MA].commutativeGroup
}
