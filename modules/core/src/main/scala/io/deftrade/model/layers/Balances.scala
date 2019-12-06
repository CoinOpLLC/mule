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

import time._, market.Frequency, money._, implicits._

import cats.implicits._
import cats.{ Invariant, Monad }
import cats.kernel.CommutativeGroup
import feralcats.instances._

import cats.effect.Sync
import fs2.{ Pipe, Stream }

import eu.timepit.refined
import refined.auto._

import io.circe.Json

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
// import spire.syntax.field._

import scala.language.higherKinds

/**
  * Double entry [[Balance]] calculation from a [[fs2.Stream]] of [[Ledger.Transaction]]s.
  *
  * When summing Transactions, this module slice implements the algebra which
  * maintains all the accounting identities.
  *
  * These are the terms and identities as '''we''' use them:
  *
  * {{{
  *     Debits := Assets + Expenses                  // accounting definition
  *     Credits := Liability + Revenue               // accounting definition
  *     Debits === Credits                           // accounting identity
  *     Assets === Liabilities                       // balance sheet identity
  *     Assets + Expenses === Liabilities + Revenue  // substituting
  *     Income := Revenue net Expenses               // the "bottom line"
  *     Liabilities := Debt + Equity                 // always one or the other
  *     RetainedEarnings = Income net Distributions  // business keeps what partners don't take
  *     Equity :=                                    // total value of partners' stakes
  *       ShareCapital                               // total raised across all rounds
  *     + Reserves                                   // you never know
  *     + RetainedEarnings                           // add to book value of partners' equity
  * }}}
  *
  */
trait Balances { self: Ledger with Accounting with ModuleTypes =>

  import AccountingKey.implicits._

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
    ): Option[(AccountMap[D, CCY], AccountMap[C, CCY])] =
      (b.debits, b.credits).some
  }

  /**
    * There are exactly two transformations of a `TrialBalance`
    * and an `amount: Mny[C]` which result in another legal `TrialBalance`:
    *   - grow (shrink) balance by amount
    *   - constant balance; swap amount between keys within debits (credits)
    *
    * These are broken out into separate methods: TODO: consider unfying.
    */
  sealed abstract case class TrialBalance[C] private (
      override val debits: Debits[C],
      override val credits: Credits[C]
  ) extends Balance(debits, credits) {

    /** */
    final def updated(
        keys: DebitCreditKey[Debit, Credit],
        amount: Mny[C]
    )(
        implicit C: Currency[C]
    ): TrialBalance[C] =
      TrialBalance(
        debits |+| (keys.debits priced -amount),
        credits |+| (keys.credits priced amount)
      )

    /** */
    final def swapped[T <: AccountingKey](
        keys: SwapKey[T],
        amount: Mny[C]
    )(
        implicit C: Currency[C]
    ): TrialBalance[C] = {
      val am: AccountMap[AccountingKey, C] = (SwapKey accountMap (keys, amount)).widenKeys
      TrialBalance(
        debits |+| (am collectKeys Debit.unapply),
        credits |+| (am collectKeys Credit.unapply)
      )
    }

    /** */
    final def cashBooks: CashBookSet[C] = ???

    /** */
    final def accrualBooks: AccrualBookSet[C] = ???
  }

  /**
    * Only public interface for creation is transformation, starting with `empty`.
    */
  object TrialBalance {

    /** */
    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): TrialBalance[C] =
      new TrialBalance(debits, credits) {}

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
    def from[F[_]: Sync, C: Currency](
        period: Period,
        cratchit: Transaction => Stream[F, DoubleEntryKey]
    ): Pipe[F, Transaction, TrialBalance[C]] =
      // extract the price from the Transaction
      //  TODO: how to guarantee this can be done in the general case
      // create a DoubleEntryKey for it (depends on price - think about waterfall impl)
      // create a TrialBalance from the price and de keys
      // fold that TrialBalance into the running sum
      ???
  }

  /**
    * Note: These are a mixture of cash and accrual items when "raw".
    * A cash account can be determined from its `Instrument.Key`.
    * This can be used to create a filter for `CashFlowStatement`s.
    * FIXME this is BS
    */
  sealed abstract case class IncomeStatement[C] private (
      val expenses: Expenses[C],
      val revenues: Revenues[C]
  ) extends Balance(expenses, revenues)

  /** */
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
    *
    * - FIXME: isn't the typing too restrictive here? Consider cash capital transactions.
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

  /**
    * Should really be called
    [[https://en.wikipedia.org/wiki/Statement_of_changes_in_equity Statement of Changes in Equity]],
    * but that is judged insufficienty consise and regular.
    *
    * Since there is no other equity stament, this shall be it.
    *
    * Nota Bene the (arithmetic) indifference of the equity holder to equity
    * transactions at market price: their book value is unaffected by such
    * transactions.
    *
    * Change in Equity value: `Credits net Debits`, as usual for a [[Balance]]
    *
    *   - Debits
    *       - Dividends paid (per share)
    *       - Share buy-backs (premium over book value per share)
    *
    *   - Credits
    *       - Shares issued (premium over book value per share)
    *       - Comprehensive Income (per share)
    */
  sealed abstract case class EquityStatement[C] private (
      override val debits: Debits[C],
      override val credits: Credits[C]
  ) extends Balance(debits, credits)

  /** */
  object EquityStatement {

    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): EquityStatement[C] = new EquityStatement(debits, credits) {}

    /** */
    implicit def equityStatementCommutativeGroup[C: Currency]: CommutativeGroup[EquityStatement[C]] =
      (Invariant[CommutativeGroup] imap CommutativeGroup[(Debits[C], Credits[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /** */
  sealed trait BookSet[C] {

    /** */
    type Repr <: BookSet[C]

    /** */
    def asOf: LocalDate

    /** */
    def period: Period

    /** `previous.asOf === this.asOf - this.Period` */
    def previous: Repr

    /** */
    def cs: CashFlowStatement[C]

    /** */
    def bs: BalanceSheet[C]

    /** */
    def es: EquityStatement[C]
  }

  /** */
  sealed abstract case class CashBookSet[C] private (
      asOf: LocalDate,
      period: Period,
      previous: CashBookSet[C],
      cs: CashFlowStatement[C],
      bs: BalanceSheet[C],
      es: EquityStatement[C],
  ) extends BookSet[C] {

    /** */
    final type Repr = CashBookSet[C]
  }

  /** */
  object CashBookSet {

    /** */
    private[Balances] def apply[F[_], C: Currency](
        asOf: LocalDate,
        period: Period,
        previous: CashBookSet[C],
        cs: CashFlowStatement[C],
        bs: BalanceSheet[C],
        es: EquityStatement[C],
    ): CashBookSet[C] =
      new CashBookSet(asOf, period, previous, cs, bs, es) {}

    /** TODO: restricted set of DoubleEntryKeys for cash books? */
    def pipe[F[_]: Sync, C: Currency](
        period: Period,
        cratchit: Transaction => Stream[F, DoubleEntryKey]
    ): Pipe[F, Transaction, CashBookSet[C]] = ???
  }

  /** */
  sealed abstract case class AccrualBookSet[C] private (
      asOf: LocalDate,
      period: Period,
      previous: AccrualBookSet[C],
      cs: CashFlowStatement[C],
      is: IncomeStatement[C],
      bs: BalanceSheet[C],
      es: EquityStatement[C],
  ) extends BookSet[C] {

    /** */
    final type Repr = AccrualBookSet[C]
  }

  /** */
  object AccrualBookSet {

    /** */
    private[Balances] def apply[C: Currency](
        asOf: LocalDate,
        period: Period,
        previous: AccrualBookSet[C],
        cs: CashFlowStatement[C],
        is: IncomeStatement[C],
        bs: BalanceSheet[C],
        es: EquityStatement[C],
    ): AccrualBookSet[C] =
      new AccrualBookSet(asOf, period, previous, cs, is, bs, es) {}

    /** */
    def pipe[F[_]: Sync, C: Currency](
        period: Period,
        cratchit: Transaction => Stream[F, DoubleEntryKey]
    ): Pipe[F, Transaction, AccrualBookSet[C]] = ???
  }
}
