package io.deftrade

/**
  * Financial entity archetypes.
  */
package object model extends model.Api[BigDecimal, Double] {

  type Result[T] = cats.data.Validated[Fail, T]

  type AccountType = enums.AccountType

  type DebitAccount  = enums.DebitAccount
  type CreditAccount = enums.CreditAccount

  type Asset = enums.Asset
  lazy val Asset = enums.Asset

  type Expense = enums.Expense
  lazy val Expense = enums.Expense

  type Revenue = enums.Revenue
  lazy val Revenue = enums.Revenue

  type Liability = enums.Liability
  lazy val Liability = enums.Liability

  type Equity = enums.Equity
  lazy val Equity = enums.Equity

}
