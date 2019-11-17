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

import model.layers._

/**
  * Financial types and methods.
  *
  * This package object is where the policy decision to use [[scala.math.BigDecimal]]
  * for [[money.Money]], and [[scala.Double]] for other [[layers.ModuleTypes.Quantity]]s, is made.
  *
  * Also, here we choose generic tax accounting for entities treated as partnerships.
  *
  * Different objects, package or otherwise, could make different policy decisions.
  *

 - CountryCode
 - Market
 - LegalEntity
 - Account

 - Instrument
  - Bond
  - CommonStock
  - PreferredStock
  - Index
  - etc

 - Folio
    - Wallet
 - Transaction
    - Order
    - Execution

 - OMS
 - MDS

  */
package object model
/*
  types we all need to agree on:
     */
    extends ModuleTypes.Aux[
      /* type MonetaryAmount = */ BigDecimal,
      /* type Quantity       = */ Double
    ]
    //
    // the full stack of layered capabilitities
    //
    with Ledger          // possibly distributed, possibly anonymous
    with Accounting      // debits, credits, and all that
    with Balances        // depends only on generic Accounting
    with MarketData      // WIP; IBRK will be first integration
    with OrderManagement // WIP; IBRK will be first integration
    //
    // PII firewalling simplified by eliminating dependencies:
    // Accounts can be commented out!
    //
    with Accounts // binding of legal entities to folios */
    //
    // necessary package level configuration
    //
    with IRS1065 // replace or enhance as necessary
