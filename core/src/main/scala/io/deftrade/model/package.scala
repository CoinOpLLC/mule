package io.deftrade

/**
  * Financial entity archetypes.
  */
package object model extends model.Api[BigDecimal, Double] {

  type AccountType = enums.AccountType

  type DebitAccount  = enums.DebitAccount
  type CreditAccount = enums.CreditAccount

  type Asset = enums.Asset
  lazy val Asset = enums.Asset
  lazy val Cash  = Asset.Cash // Everyone needs Cash.

  type LOQ = enums.LOQ
  lazy val LOQ = enums.LOQ

  type Liability = enums.Liability
  lazy val Liability = enums.Liability

  type Equity = enums.Equity
  lazy val Equity = enums.Equity

  type XOP = enums.XOP
  lazy val XOP = enums.XOP

  type Expense = enums.Expense
  lazy val Expense = enums.Expense

  type Profit = enums.Profit
  lazy val Profit = enums.Profit

  type Revenue = enums.Revenue
  lazy val Revenue = enums.Revenue

}
