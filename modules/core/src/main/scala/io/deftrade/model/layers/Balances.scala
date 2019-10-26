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
package layers

import money._, time._

import cats.implicits._
import cats.{ Foldable, Invariant, Monad, SemigroupK }
import cats.kernel.CommutativeGroup
import feralcats.instances._

import eu.timepit.refined
import refined.auto._

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
import spire.syntax.field._

import scala.language.higherKinds

/**
  * Double entry [[Balance]] calculation from a sequence of [[Ledger.Transaction]]s.
  *
  * Recall the fundamental equation of double entry bookkeeping:
  *
  *   - `Debits === Credits`
  *
  * Expanding:
  *
  *   - `Assets + Expenses === Liabilities + Equity + Revenues`
  *
  * When summing Transactions, this "cake slice" module implements the algebra which
  * maintains the above equality.
  *
  */
trait Balances {
  self: Ledger with Accounting with ModuleTypes =>

  import AccountMap.implicits._

  /** */
  sealed trait BalanceLike extends Product with Serializable {

    /** */
    type CurrencyType

    /** */
    final def currency(implicit C: Currency[CurrencyType]): Currency[CurrencyType] = C

    /** */
    type DebitType <: Debit

    /** */
    def debits: AccountMap[DebitType, CurrencyType]

    /** */
    type CreditType <: Credit

    /** */
    def credits: AccountMap[CreditType, CurrencyType]
  }

  /** specify key types */
  sealed abstract class Balance[D <: Debit, C <: Credit, CCY] private[Balances] (
      ds: AccountMap[D, CCY],
      cs: AccountMap[C, CCY]
  ) extends BalanceLike {

    import io.deftrade.implicits._ // FIXME: why tf does this have to be here?

    /** */
    final type DebitType = D

    /** */
    final type CreditType = C

    /** */
    final type CurrencyType = CCY

    /** overridable */
    def debits: AccountMap[DebitType, CurrencyType] = ds

    /** overridable */
    def credits: AccountMap[CreditType, CurrencyType] = cs

    /** */
    final def net: Mny[CurrencyType] =
      credits.total - debits.total
  }

  /** */
  object Balance {

    /** Decompose into separate `Debit` and `Credit` maps. */
    def unapply[D <: Debit, C <: Credit, CCY: Currency](
        b: Balance[D, C, CCY]
    ): Option[(AccountMap[D, CCY], AccountMap[C, CCY])] = (b.debits, b.credits).some
  }

  /** Most general / least safe... */
  sealed abstract case class TrialBalance[C] private (
      override val debits: Debits[C],
      override val credits: Credits[C]
  ) extends Balance(debits, credits) {

    /** */
    def updated(dc: DebitCreditKey[Debit, Credit], amount: Mny[C])(
        implicit C: Currency[C]
    ): TrialBalance[C] =
      TrialBalance(debits |+| (dc.debits priced -amount), credits |+| (dc.credits priced amount))

    /** */
    def swapped[T <: AccountingKey](sk: SwapKey[T], amount: Mny[C])(
        implicit C: Currency[C]
    ): TrialBalance[C] = {
      val am: AccountMap[AccountingKey, C] = (SwapKey accountMap (sk, amount)).widenKeys
      TrialBalance(
        debits |+| (am collectKeys Debit.unapply),
        credits |+| (am collectKeys Credit.unapply)
      )
    }

    /** */
    def partition(implicit C: Currency[C]): (IncomeStatement[C], BalanceSheet[C]) =
      (
        IncomeStatement(debits collectKeys Expense.unapply, credits collectKeys Revenue.unapply),
        BalanceSheet(debits collectKeys Asset.unapply, credits collectKeys Liability.unapply)
      )
  }

  /** */
  object TrialBalance {

    /** */
    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): TrialBalance[C] = new TrialBalance(debits, credits) {}

    /** */
    def empty[C: Currency]: TrialBalance[C] =
      apply(
        debits = AccountMap.empty[Debit, C],
        credits = AccountMap.empty[Credit, C],
      )

    /** */
    implicit def trialBalanceCommutativeGroup[C: Currency]: CommutativeGroup[TrialBalance[C]] =
      (Invariant[CommutativeGroup] imap CommutativeGroup[(Debits[C], Credits[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /** */
    def from[CC[_]: Foldable, C: Currency](
        marker: TradePricer[C]
    )(
        xs: CC[Transaction]
    ): TrialBalance[C] = ??? // xs.sum // FIXME this is all I should have to say!

  }

  /**
    * Note: These are a mixture of cash and accrual items when "raw".
    * A cash account can be determined from its `Instrument.Key`.
    * This can be used to create a filter for `CashFlowStatement`s.
    */
  sealed abstract case class IncomeStatement[C] private (
      val expenses: Expenses[C],
      val revenues: Revenues[C]
  ) extends Balance(expenses, revenues) {

    /** */
    def partition(implicit ci: Wallet.Aux[C]): (IncomeStatement[C], CashFlowStatement[C]) =
      ???
  }

  /**
    *
    */
  object IncomeStatement {

    private[model] def apply[C: Currency](
        expenses: Expenses[C],
        revenues: Revenues[C]
    ): IncomeStatement[C] = new IncomeStatement(expenses, revenues) {}

    /** */
    implicit def incomeStatementCommutativeGroup[C: Currency]: CommutativeGroup[IncomeStatement[C]] =
      (Invariant[CommutativeGroup] imap CommutativeGroup[(Expenses[C], Revenues[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /**
    * Follow the `Money`.
    * - Operations
    * - Investment
    * - Financing
    * - TODO: should cash accounting get their own flavors? for now reuse `Revenue` and `Expense`.
    */
  sealed abstract case class CashFlowStatement[C] private (
      val outflows: Expenses[C],
      val inflows: Revenues[C]
  ) extends Balance(outflows, inflows)

  /**  */
  object CashFlowStatement {

    /** */
    private[model] def apply[C: Currency](
        outflows: Expenses[C],
        inflows: Revenues[C]
    ): CashFlowStatement[C] = new CashFlowStatement(outflows, inflows) {}

    /** */
    implicit def cashflowStatementCommutativeGroup[C: Currency]: CommutativeGroup[CashFlowStatement[C]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Expenses[C], Revenues[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /** */
  sealed abstract case class EquityStatement[C] private (wut: Null)

  /** */
  object EquityStatement

  /**
    * `BalanceSheet`s form a `CommutativeGroup`.
    *  All operations are double entry by construction.
    */
  sealed abstract case class BalanceSheet[C] private (
      val assets: Assets[C],
      val liabilities: Liabilities[C]
  ) extends Balance(assets, liabilities)

  /** */
  object BalanceSheet {

    /** */
    private[model] def apply[C: Currency](
        assets: Assets[C],
        liabilities: Liabilities[C]
    ): BalanceSheet[C] = new BalanceSheet(assets, liabilities) {}

    /** */
    implicit def balanceCommutativeGroup[C: Currency]: CommutativeGroup[BalanceSheet[C]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Assets[C], Liabilities[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /** */
  sealed trait BookSet[C] {

    /** */
    def asOf: LocalDate

    /** */
    def period: Period

    /** */
    final def beginning: LocalDate = asOf - period

    /** */
    def cs: CashFlowStatement[C]

    /** */
    def bs: BalanceSheet[C]

    /** FIXME this must evole */
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): BookSet[C]
  }

  /** */
  sealed abstract case class CashBookSet[C](
      asOf: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      bs: BalanceSheet[C]
  ) extends BookSet[C] {

    /** FIXME: sketchy; comprehend streams? */
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???

    /**
      * Cratchit needs to look at the current state of the books
      * in order to properly allocate balance sheet items (in the most general case)
      *
      * FIXME: Need to make polymorphic (some how).
      */
    def deltaFrom(
        pt: PricedTrade[C],
        dek: DoubleEntryKey,
        meta: Transaction.Meta
    ): DeltaCashBooks[C] = ???
  }

  /** */
  object CashBookSet {
    def apply[C: Currency](
        asOf: LocalDate,
        period: Period,
        cs: CashFlowStatement[C],
        bs: BalanceSheet[C]
    ): CashBookSet[C] = new CashBookSet(asOf, period, cs, bs) {}
  }

  /** */
  sealed abstract case class AccrualBookSet[C](
      asOf: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      is: IncomeStatement[C],
      bs: BalanceSheet[C]
  ) extends BookSet[C] {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  /** */
  object AccrualBookSet {
    def apply[C: Currency](
        asOf: LocalDate,
        period: Period,
        cs: CashFlowStatement[C],
        is: IncomeStatement[C],
        bs: BalanceSheet[C]
    ): AccrualBookSet[C] = new AccrualBookSet(asOf, period, cs, is, bs) {}
  }

  /** */
  def trialBalance[F[_]: Foldable: Monad: SemigroupK, C: Currency](
      ts: F[Transaction]
  ): TrialBalance[C] =
    // price the transaction
    // create a DoubleEntryKey for it (depends on price - think about waterfall impl)
    // create a TrialBalance from the price and de keys
    // fold that TrialBalance into the running sum
    ???

  /** FIXME: not sure this signature makes sense as it stands */
  def breakdown[C: Currency: Wallet.Aux](
      prior: BalanceSheet[C],
      delta: BalanceSheet[C], // delta and raw come from TrialBalance
      raw: IncomeStatement[C] // mixed cash and accrual
  ): (CashFlowStatement[C], EquityStatement[C]) =
    ???

  /**  */
  type DeltaCashBooks[C] = (CashFlowStatement[C], BalanceSheet[C])

  /** */
  type DeltaAccrualBooks[C] = (IncomeStatement[C], CashFlowStatement[C], BalanceSheet[C])
}
