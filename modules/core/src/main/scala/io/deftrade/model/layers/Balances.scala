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
package model.layers

import syntax._, time._, money._, keyval._, model.instances._

import cats.implicits._
import cats.{ Invariant }
import cats.kernel.CommutativeGroup
import cats.data.NonEmptyList

import cats.effect.Sync
import fs2.{ Pipe, Stream }

import eu.timepit.refined
import refined.auto._

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
// import spire.syntax.field._

/** Double entry [[Balance]] calculation from a [[fs2.Stream]] of [[Ledger.Transaction]]s.
  */
trait Balances { self: ModuleTypes with Ledger with Accounting =>

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
  object Balance {

    final type Entry = (AccountingKey, MonetaryAmount)

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

    /** Decompose into separate `Debit` and `Credit` maps. */
    def unapply[DB <: Debit, CR <: Credit, C: Currency](
        b: Balance.Aux[DB, CR, C]
    ): Option[(AccountingMap[DB, C], AccountingMap[CR, C])] =
      (b.debits, b.credits).some
  }

  /**
    */
  sealed abstract class BalanceStores[B <: Balance]
      extends ValueStores.Codec(BalanceStores.encode[B], BalanceStores.decode[B])

  /**
    */
  object BalanceStores {
    def encode[B <: Balance]: B => NonEmptyList[Balance.Entry] = _ => ???
    def decode[B <: Balance]: NonEmptyList[Balance.Entry] => B = _ => ???
  }

  /**
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
        debits |+| (keys.debits priced -amount).toSortedMap,
        credits |+| (keys.credits priced amount).toSortedMap
      )

    /**
      */
    final def swapped[T <: AccountingKey](
        keys: SwapKey[T],
        amount: Money[C]
    )(implicit C: Currency[C]): TrialBalance[C] = {
      val am: AccountingMap[AccountingKey, C] = (SwapKey.accountMap(keys, amount)).widenKeys
      TrialBalance(
        debits |+| (am collectKeys {
          case d: Debit => d.some
          case _        => none
        }),
        credits |+| (am collectKeys {
          case c: Credit => c.some
          case _         => none
        })
      )
    }

    /** TODO: consider: what if these were just views?
      */
    final def cashBooks: CashReport.Aux[C] = ???

    /**
      */
    final def accrualBooks: AccrualReport.Aux[C] = ???
  }

  /** Only public interface for creation is transformation, starting with `empty`.
    */
  object TrialBalance {

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
      //  TODO: how to guarantee this can be done in the general case
      ???
  }

  /**
    */
  sealed trait IncomeStatement extends Balance {
    def expenses: Expenses[CurrencyTag]
    def revenues: Revenues[CurrencyTag]
  }

  case object IncomeStatement extends BalanceStores[IncomeStatement] {

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
    */
  sealed trait CashFlowStatement extends Balance {
    def outflows: Debits[CurrencyTag]
    def inflows: Credits[CurrencyTag]
  }

  /**
    */
  case object CashFlowStatement extends BalanceStores[CashFlowStatement] {

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
  case object BalanceSheet extends BalanceStores[BalanceSheet] {

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

  /** Often formally known as Statement of Changes in Equity.
    */
  sealed trait EquityStatement extends Balance

  /** TODO: implement `Period` as a secondary index?
    */
  case object EquityStatement extends BalanceStores[EquityStatement] {

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
    type ReprId <: Reports.Id

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

  /** Placeholder
    *
    * TODO: Footnotes table or column?
    */
  object Report {

    /**
      */
    sealed abstract class Aux[C] extends Report {
      final type CurrencyTag = C
    }
  }

  case object Reports extends KeyValueStores.KV[Folios.Key, Report]

  /**
    */
  sealed trait CashReport extends Report {
    final type ReprId = CashReports.Id
  }

  /**
    */
  object CashReport {

    /**
      */
    sealed abstract case class Aux[C] private[CashReport] (
        asOf: Instant,
        period: Period,
        previous: CashReports.Id,
        cs: CashFlowStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ) extends Report.Aux[C]
        with CashReport

    /**
      */
    private[model] def apply[F[_], C: Currency](
        asOf: Instant,
        period: Period,
        previous: CashReports.Id,
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

  case object CashReports extends KeyValueStores.KV[Folios.Key, CashReport]

  /**
    */
  sealed trait AccrualReport extends Report {

    final type ReprId = AccrualReports.Id

    /**
      */
    sealed abstract case class View private[AccrualReport] (
        cs: CashFlowStatement.Aux[CurrencyTag],
        is: IncomeStatement.Aux[CurrencyTag],
        bs: BalanceSheet.Aux[CurrencyTag],
        es: EquityStatement.Aux[CurrencyTag]
    )

    def is: IncomeStatement.Id
  }

  /**
    */
  object AccrualReport {

    sealed abstract case class Aux[C] private[AccrualReport] (
        asOf: Instant,
        period: Period,
        previous: AccrualReports.Id,
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
        previous: AccrualReports.Id,
        cs: CashFlowStatement.Id,
        is: IncomeStatement.Id,
        bs: BalanceSheet.Id,
        es: EquityStatement.Id
    ): AccrualReport =
      new Aux(asOf, period, previous, cs, is, bs, es) {}

    /**
      */
    final def report[F[_]: Sync, C: Currency](
        previous: AccrualReports.Id,
        period: Period,
        treatment: Transaction => Stream[F, DoubleEntryKey]
    ): Stream[F, Transactions.Id] => F[AccrualReports.Id] =
      ???
  }

  case object AccrualReports extends KeyValueStores.KV[Folios.Key, AccrualReport] {
    // lazy val Key = Folios.Key
  }
}
