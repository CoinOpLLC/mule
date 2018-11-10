package io.deftrade

/**
  * Financial entity archetypes.
  */
package object model extends model.Api[BigDecimal, Double] {

  override type AccountType = enums.AccountType

  override type Debit  = enums.DebitAccount
  override type Credit = enums.CreditAccount

  override type Asset = enums.Asset
  lazy val Asset = enums.Asset
  lazy val Cash  = Asset.Cash // Everyone needs Cash.

  type LOQ = enums.LOQ
  lazy val LOQ = enums.LOQ

  override type Liability = enums.Liability
  lazy val Liability = enums.Liability

  override type Equity = enums.Equity
  lazy val Equity = enums.Equity

  override type XOP = enums.XOP
  lazy val XOP = enums.XOP

  override type Expense = enums.Expense
  lazy val Expense = enums.Expense

  override type Profit = enums.Profit
  lazy val Profit = enums.Profit

  override type Revenue = enums.Revenue
  lazy val Revenue = enums.Revenue

}
