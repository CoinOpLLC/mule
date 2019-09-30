package io.deftrade

/**
  * Financial business objects and functions.
  *
  * This package object is where the policy decision to use [[scala.math.BigDecimal]]
  * for [[money.Money]], and [[scala.Double]] for other [[ModuleTypes.Quantity]]s, is made.
  *
  * Also, here we choose generic tax accounting for entities treated as partnerships.
  *
  * Different objects, package or otherwise, could make different policy decisions.
  */
package object model
    extends ModuleTypes.Aux[BigDecimal, Double] // implicit resolution does the rest
    with AccountingIRS1065
    with Ledger
    with Pricing
    with Balances
    with Trading
