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
  *   ==Mandate:==
  *   Model the monetary and currency conversion use cases typical of
  *   '''financial market participants''': banks, certainly, but also hedge funds,
  *   private equity funds, insurance companies, mortgage companies, etc.
  *
  *   ==Requirements:==
  *   - scalazzi compliant
  *   - plays well with cats
  *   - exploit java Currency support for [[https://en.wikipedia.org/wiki/ISO_4217 ISO 4217]]
  *   - take some ''inspiration'' from `squants.market`
  *   - comprehend the facilities provided by other FOSS packages
  *       - [[https://www.joda.org/joda-money/ Joda Money]] of course
  *       - [[https://github.com/Appendium/objectlabkit ObjectLabKit]]
  *
  *   ==Implementation:==
  *   - [[Money]] is a value class type constructor for type parameter `[N: Financial]`
  *   - distinct types for each currency
  *       - summon implicit [[Currency]]`[C`] typeclass instance given a currency type `C`
  *   - abstract over currencies for single implicit `cats.CommutativeGroup` function
  *   - few dependencies:
  *       - `typelevel algebra` (which includes `spire`)
  *       - `Enumeratum` to walk thru implemented currency codes
  *       - integration with `Refined` via `RefType[F[_,_]]`
  *
  *   TODO: track evolving
  * [[https://en.wikipedia.org/wiki/ISO_4217#Cryptocurrencies crypto support]]
  *
  */
package object money {

  implicit def financialCommutativeGroup[MA: Financial]: CommutativeGroup[MA] =
    Financial[MA].commutativeGroup

}
