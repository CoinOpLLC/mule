package io.deftrade

/**
  * Financial business objects and functions.
  *
  * This package object is where the policy decision to use [[scala.math.BigDecimal]]
  * for [[money.Money]], and [[scala.Double]] for other [[Ledger.Quantity]]s, is made.
  */
package object model extends model.Trading[BigDecimal, Double]
