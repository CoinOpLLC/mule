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
import spire.math.Fractional

/**
  *   requirements for currency / money:
  *   - scalazzi-9000-complient™, cats-friendly™
  *   - take some _inspiration_ from `squants.market` (emphasis _mine_)
  *   - comprehend the best of other FOSS offerings (esp the moribund ones!)
  *      -  these chaps https://github.com/Appendium/objectlabkit
  *      -  but lose the whole "is market convention" thing - yagni
  *
  *   implementation
  *   - minimal to no overhead (value classes on [N: Financial])
  *   - distinct types for each currency
  *   - summon implicit Currency[C] typeclass instance given a currency type C
  *   - exploit java Currency support
  *   - abstract over currencies for single implicit `cats.CommutativeGroup` function
  *   - Currency enum as factory pattern - use to "print legit money"
  *   - no dependencies other than `typelevel algebra` (which includes `spire`)
  *      - ...and integration with `Refined` via `RefType[F[_,_]]`,
  *      - ...and `Enumeratum` to walk thru implemented currency codes
  *   - use the currency conversion / cross conventions typical of financial market participants.
  *      - e.g. banks, and long/short credit hedge funds, which aren't much different)
  *
  *   https://en.wikipedia.org/wiki/ISO_4217
  *   TODO: treatment of what's expected to be imported implicitly
  *   https://en.wikipedia.org/wiki/ISO_4217#Cryptocurrencies
  */
package object money {

  implicit def financialCommutativeGroup[MA: Financial]: CommutativeGroup[MA] =
    Financial[MA].commutativeGroup

  implicit def financialFractional[MA: Financial]: Fractional[MA] =
    Financial[MA].fractional
}
