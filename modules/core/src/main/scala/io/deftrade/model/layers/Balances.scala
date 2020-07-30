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

  import AccountingKey.implicits._

  /**
    */
  sealed trait Balance {

    /**
      */
    type CurrencyType

    /**
      */
    type DebitType <: Debit

    /**
      */
    type CreditType <: Credit

    /**
      */
    def debits: AccountingMap[DebitType, CurrencyType]

    /**
      */
    def credits: AccountingMap[CreditType, CurrencyType]

    /**
      */
    final def currency(implicit C: Currency[CurrencyType]): Currency[CurrencyType] = C

    /**
      */
    final def net(implicit C: Currency[CurrencyType]): Money[CurrencyType] =
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
      final type DebitType = DB

      /**
        */
      final type CreditType = CR

      /**
        */
      final type CurrencyType = C
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
    final def cashBooks: CashBookSet[C] = ???

    /**
      */
    final def accrualBooks: AccrualBookSet[C] = ???
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
    def expenses: Expenses[CurrencyType]
    def revenues: Revenues[CurrencyType]
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
    def outflows: Debits[CurrencyType]
    def inflows: Credits[CurrencyType]
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
    def assets: Assets[CurrencyType]
    def liabilities: Liabilities[CurrencyType]
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
  sealed trait BookSet[C] {

    /**
      */
    type Repr <: BookSet[C]

    /**
      */
    def asOf: LocalDate

    /**
      */
    def period: Period

    /** `previous.asOf === this.asOf - this.Period` */
    def previous: Repr

    /**
      */
    def cs: CashFlowStatement.Id

    /**
      */
    def bs: BalanceSheet.Id

    /**
      */
    def es: EquityStatement.Id
  }

  /**
    * Placeholder
    */
  object BookSet {}

  /**
    */
  sealed abstract case class CashBookSet[C] private (
      asOf: LocalDate,
      period: Period,
      previous: CashBookSet[C],
      cs: CashFlowStatement.Id,
      bs: BalanceSheet.Id,
      es: EquityStatement.Id
  ) extends BookSet[C] {

    /**
      */
    final type Repr = CashBookSet[C]
  }

  /** FIXME: existential types stink
    */
  object CashBookSet extends WithKey.Aux[Folio.Key, CashBookSet[_]] {

    lazy val Key = Folio.Key

    /**
      */
    private[Balances] def apply[F[_], C: Currency](
        asOf: LocalDate,
        period: Period,
        previous: CashBookSet[C],
        cs: CashFlowStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ): CashBookSet[C] =
      new CashBookSet(asOf, period, previous, cs, bs, es) {}

    /** TODO: restricted set of DoubleEntryKeys for cash books? */
    def pipe[F[_]: Sync, C: Currency](
        period: Period,
        cratchit: Transaction => Stream[F, DoubleEntryKey]
    ): Pipe[F, Transaction, CashBookSet[C]] = ???
  }

  /**
    */
  sealed abstract case class AccrualBookSet[C] private (
      asOf: LocalDate,
      period: Period,
      previous: AccrualBookSet[C],
      cs: CashFlowStatement.Id,
      is: IncomeStatement.Id,
      bs: BalanceSheet.Id,
      es: EquityStatement.Id
  ) extends BookSet[C] {

    /**
      */
    final type Repr = AccrualBookSet[C]
  }

  /**
    */
  object AccrualBookSet extends WithKey.Aux[Folio.Key, AccrualBookSet[_]] {

    lazy val Key = Folio.Key

    case class View[C](
        cs: CashFlowStatement.Aux[C],
        is: IncomeStatement.Aux[C],
        bs: BalanceSheet.Aux[C],
        es: EquityStatement.Aux[C]
    )

    /** wip */
    def view[F[_], C: Currency](abss: AccrualBookSets.Store[F])(abs: AccrualBookSet.Id): View[C] =
      ???

    /**
      */
    private[Balances] def apply[C: Currency](
        asOf: LocalDate,
        period: Period,
        previous: AccrualBookSet[C],
        cs: CashFlowStatement.Id,
        is: IncomeStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ): AccrualBookSet[C] =
      new AccrualBookSet(asOf, period, previous, cs, is, bs, es) {}

    /**
      */
    def pipe[F[_]: Sync, C: Currency](
        period: Period,
        cratchit: Transaction => Stream[F, DoubleEntryKey]
    ): Pipe[F, Transaction, AccrualBookSet[C]] = ???

    /**
      */
    def closeTheBooks[F[_]: Sync, C: Currency](
        previous: AccrualBookSet.Id,
        period: Period,
        treatment: Transaction.Id => Stream[F, DoubleEntryKey]
    ): Stream[F, Transaction.Id] => F[AccrualBookSet.Id] =
      ???
  }

  /**
    */
  lazy val AccrualBookSets = KeyValueStore of AccrualBookSet
}
