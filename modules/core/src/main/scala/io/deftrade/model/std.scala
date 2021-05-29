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
package model

import model.layers._
import model.augments._

import cats.implicits._
import cats.effect.{ IO }

/**
  * Records and computations defining a layered set of financial domain models and services.
  */
trait std
    extends ModuleTypes.Aux[
      /* type IO[_]          = */ IO,
      /* type MonetaryAmount = */ BigDecimal,
      /* type Quantity       = */ Double
    ]

    // the full stack of layered capabilities
    with Person          //
    with Paper           //
    with Ledger          // possibly distributed
    with Accounting      // debits, credits, and all that
    with Balances        // depends only on generic Accounting
    with MarketData      // WIP; IBRK will be first integration
    with OrderManagement // WIP; IBRK will be first integration
    //
    // PII firewalling simplified by eliminating dependencies:
    // `Accounts` layer can be commented out!
    //
    with Accounts // binding of legal entities to sets of positions
    //
    // necessary package level augmentation
    //
    with IRS1065 // replace or enhance as necessary
    // //
    with csvStores { // replace or enhance as necessary
}

object std extends std
