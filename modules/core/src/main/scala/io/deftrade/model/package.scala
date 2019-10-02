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
  */
package object model
    extends ModuleTypes.Aux[BigDecimal, Double]
    with AccountingIRS1065
    with Ledger
    with Balances
    with MarketData
    with OrderManagement
