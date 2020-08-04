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

import syntax._, time._, money._, keyval._

import cats.implicits._
import cats.{ Invariant }
import cats.kernel.CommutativeGroup

import cats.effect.Sync
import fs2.{ Pipe, Stream }

import eu.timepit.refined
import refined.auto._

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
// import spire.syntax.field._

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
  */
trait Balances { self: Ledger with Accounting with ModuleTypes =>

  import AccountingKey.syntax._

  /**
    */
  sealed trait Balance {

    /**
      */
    type CurrencyTag

    /**
      */
    type DebitType <: Debit

    /**
      */
    type CreditType <: Credit

    /**
      */
    def debits: AccountingMap[DebitType, CurrencyTag]

    /**
      */
    def credits: AccountingMap[CreditType, CurrencyTag]

    /**
      */
    final def currency(implicit C: Currency[CurrencyTag]): Currency[CurrencyTag] = C

    /**
      */
    final def net(implicit C: Currency[CurrencyTag]): Money[CurrencyTag] =
      credits.total - debits.total

  }

  /**
    */
  // object Balance extends WithKey.Aux[(Folio.Key, LocalDate, Period), BalanceLike] {
  object Balance extends WithKey.Aux[Folio.Key, Balance] {

    /** Exact type of both keys is specified.
      */
    sealed abstract class Aux[DB <: Debit, CR <: Credit, C] private[Balances] (
        override val debits: AccountingMap[DB, C],
        override val credits: AccountingMap[CR, C]
    ) extends Balance {

      /**
        */
      final type CurrencyTag = C

      /**
        */
      final type DebitType = DB

      /**
        */
      final type CreditType = CR
    }

    lazy val Key = Folio.Key

    /** Decompose into separate `Debit` and `Credit` maps. */
    def unapply[DB <: Debit, CR <: Credit, C: Currency](
        b: Balance.Aux[DB, CR, C]
    ): Option[(AccountingMap[DB, C], AccountingMap[CR, C])] =
      (b.debits, b.credits).some
  }

  /**
    * There are exactly two transformations of a `TrialBalance`
    * and an `amount: Money[C]` which result in another legal `TrialBalance`:
    *   - grow (shrink) balance by amount
    *   - constant balance; swap amount between keys within debits (credits)
    *
    * These are broken out into separate methods: TODO: consider unfying.
    */
  sealed abstract case class TrialBalance[C] private (
      override val debits: Debits[C],
      override val credits: Credits[C]
  ) extends Balance.Aux(debits, credits) {

    /**
      */
    final def updated(
        keys: DebitCreditKey,
        amount: Money[C]
    )(implicit C: Currency[C]): TrialBalance[C] =
      TrialBalance(
        debits |+| (keys.debits priced -amount),
        credits |+| (keys.credits priced amount)
      )

    /**
      */
    final def swapped[T <: AccountingKey](
        keys: SwapKey[T],
        amount: Money[C]
    )(implicit C: Currency[C]): TrialBalance[C] = {
      val am: AccountingMap[AccountingKey, C] = (SwapKey.accountMap(keys, amount)).widenKeys
      TrialBalance(
        debits |+| (am collectKeys Debit.unapply),
        credits |+| (am collectKeys Credit.unapply)
      )
    }

    /** TODO: consider: what if these were just views?
      */
    final def cashBooks: CashReport.Aux[C] = ???

    /**
      */
    final def accrualBooks: AccrualReport.Aux[C] = ???
  }

  /**
    * Only public interface for creation is transformation, starting with `empty`.
    */
  object TrialBalance extends WithKey.Aux[Folio.Key, TrialBalance[_]] {

    lazy val Key = Folio.Key

    /**
      */
    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): TrialBalance[C] =
      new TrialBalance(debits, credits) {}

    /**
      */
    def empty[C: Currency]: TrialBalance[C] =
      apply(
        debits = AccountingMap.empty[Debit, C],
        credits = AccountingMap.empty[Credit, C]
      )

    /**
      */
    implicit def trialBalanceCommutativeGroup[C: Currency]: CommutativeGroup[TrialBalance[C]] =
      (Invariant[CommutativeGroup] imap CommutativeGroup[(Debits[C], Credits[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /**
      */
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
    */
  sealed trait IncomeStatement extends Balance {
    def expenses: Expenses[CurrencyTag]
    def revenues: Revenues[CurrencyTag]
  }

  /**
    */
  object IncomeStatement extends WithKey.Aux[Folio.Key, IncomeStatement] {

    lazy val Key = Folio.Key

    sealed abstract case class Aux[C] private[IncomeStatement] (
        final override val expenses: Expenses[C],
        final override val revenues: Revenues[C]
    ) extends Balance.Aux(expenses, revenues)
        with IncomeStatement

    private[model] def apply[C: Currency](
        expenses: Expenses[C],
        revenues: Revenues[C]
    ): IncomeStatement = new Aux(expenses, revenues) {}
  }

  /**
    * Follow the `Money`:
    * - Operations
    * - Investment
    * - Financing
    */
  sealed trait CashFlowStatement extends Balance {
    def outflows: Debits[CurrencyTag]
    def inflows: Credits[CurrencyTag]
  }

  /**
    */
  object CashFlowStatement extends WithKey.Aux[Folio.Key, CashFlowStatement] {

    lazy val Key = Folio.Key

    /**
      */
    sealed abstract case class Aux[C] private[CashFlowStatement] (
        final override val outflows: Debits[C],
        final override val inflows: Credits[C]
    ) extends Balance.Aux(debits = outflows, credits = inflows)
        with CashFlowStatement

    /**
      */
    private[model] def apply[C: Currency](
        outflows: Debits[C],
        inflows: Credits[C]
    ): CashFlowStatement = new Aux(outflows, inflows) {}
  }

  /**
    */
  sealed trait BalanceSheet extends Balance {
    def assets: Assets[CurrencyTag]
    def liabilities: Liabilities[CurrencyTag]
  }

  /**
    */
  object BalanceSheet extends WithKey.Aux[Folio.Key, BalanceSheet] {

    lazy val Key = Folio.Key

    /**
      */
    sealed abstract case class Aux[C] private[BalanceSheet] (
        final override val assets: Assets[C],
        final override val liabilities: Liabilities[C]
    ) extends Balance.Aux(debits = assets, credits = liabilities)
        with BalanceSheet

    /**
      */
    private[model] def apply[C: Currency](
        assets: Assets[C],
        liabilities: Liabilities[C]
    ): BalanceSheet = new Aux[C](assets, liabilities) {}
  }

  /**
    * Should really be called
    *    [[https://en.wikipedia.org/wiki/Statement_of_changes_in_equity
    * Statement of Changes in Equity]],
    * but that is judged insufficienty consise and regular.
    *
    * Since there is no other equity stament, this naming causes no confusion.
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
  sealed trait EquityStatement extends Balance

  /** TODO: implement `Period` as a secondary index?
    */
  object EquityStatement extends WithKey.Aux[Folio.Key, EquityStatement] {

    lazy val Key = Folio.Key

    sealed abstract case class Aux[C] private[EquityStatement] (
        override val debits: Debits[C],
        override val credits: Credits[C]
    ) extends Balance.Aux(debits, credits)
        with EquityStatement

    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): EquityStatement = new Aux(debits, credits) {}
  }

  /**
    */
  sealed trait Report {

    /**
      */
    type CurrencyTag

    /**
      */
    type ReprId <: Report.Id

    /**
      */
    def asOf: Instant

    /**
      */
    def period: Period

    /** `previous.asOf === this.asOf - this.Period` */
    def previous: ReprId

    /**
      */
    def cs: CashFlowStatement.Id

    /**
      */
    def bs: BalanceSheet.Id

    /**
      */
    def es: EquityStatement.Id

    /**
      */
    final def currency(implicit C: Currency[CurrencyTag]): Currency[CurrencyTag] = C
  }

  /**
    * Placeholder
    */
  object Report extends WithId[(Folio.Id, Report)] {

    /**
      */
    sealed abstract class Aux[C] extends Report {
      final type CurrencyTag = C
    }
  }

  /**
    */
  sealed trait CashReport extends Report {

    /**
      */
    final type ReprId = CashReport.Id
  }

  /**
    */
  object CashReport extends WithKey.Aux[Folio.Key, CashReport] {

    /**
      */
    sealed abstract case class Aux[C] private[CashReport] (
        asOf: Instant,
        period: Period,
        previous: CashReport.Id,
        cs: CashFlowStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ) extends Report.Aux[C]
        with CashReport

    lazy val Key = Folio.Key

    /**
      */
    private[model] def apply[F[_], C: Currency](
        asOf: Instant,
        period: Period,
        previous: CashReport.Id,
        cs: CashFlowStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ): CashReport =
      new Aux[C](asOf, period, previous, cs, bs, es) {}

    /** TODO: restricted set of DoubleEntryKeys for cash books? */
    def pipe[F[_]: Sync, C: Currency](
        period: Period,
        cratchit: Transaction => Stream[F, DoubleEntryKey]
    ): Pipe[F, Transaction, CashReport] = ???
  }

  /**
    */
  sealed trait AccrualReport extends Report {

    final type ReprId = AccrualReport.Id

    /**
      */
    sealed abstract case class View private[AccrualReport] (
        cs: CashFlowStatement.Aux[CurrencyTag],
        is: IncomeStatement.Aux[CurrencyTag],
        bs: BalanceSheet.Aux[CurrencyTag],
        es: EquityStatement.Aux[CurrencyTag]
    )

    /** wip */
    def view[F[_]](abss: AccrualReports.Store[F])(abs: AccrualReport.Id): View =
      ???

    def is: IncomeStatement.Id
  }

  /**
    */
  object AccrualReport extends WithKey.Aux[Folio.Key, AccrualReport] {

    lazy val Key = Folio.Key

    sealed abstract case class Aux[C] private[AccrualReport] (
        asOf: Instant,
        period: Period,
        previous: AccrualReport.Id,
        cs: CashFlowStatement.Id,
        is: IncomeStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ) extends Report.Aux[C]
        with AccrualReport

    /**
      */
    private[Balances] def apply[C: Currency](
        asOf: Instant,
        period: Period,
        previous: AccrualReport.Id,
        cs: CashFlowStatement.Id,
        is: IncomeStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ): AccrualReport =
      new Aux(asOf, period, previous, cs, is, bs, es) {}

    /**
      */
    final def report[F[_]: Sync, C: Currency](
        previous: AccrualReport.Id,
        period: Period,
        treatment: Transaction => Stream[F, DoubleEntryKey]
    ): Stream[F, Transaction.Id] => F[AccrualReport.Id] =
      ???
  }

  /**
    */
  lazy val AccrualReports = KeyValueStore of AccrualReport
}
