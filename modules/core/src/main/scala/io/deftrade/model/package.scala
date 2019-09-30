package io.deftrade

import money.Financial

/**
  * Financial business objects and functions.
  *
  * This package object is where the policy decision to use [[scala.math.BigDecimal]]
  * for [[money.Money]], and [[scala.Double]] for other [[Ledger.Quantity]]s, is made.
  */
package object model
    extends ModuleTypes[BigDecimal, Double](ma = Financial[BigDecimal], q = Financial[Double])
    with AccountingIRS1065
    with Ledger
    with Pricing
    with Balances
    with Trading
