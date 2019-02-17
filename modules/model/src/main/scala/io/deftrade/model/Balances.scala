package io.deftrade
package model

import money._

import keys.{ AssetSwapKey, LOQSwapKey, SwapKey, UpdateKey }

import cats.{ Foldable, Invariant, Monad, MonoidK }
import cats.kernel.CommutativeGroup
import cats.implicits._
import feralcats.instances._

import eu.timepit.refined
import refined.auto._

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
import spire.math.{ Fractional }
import spire.syntax.field._

import scala.language.higherKinds

/**
  * Double entry `Balance` calculation from a sequence of `Transaction`s.
  *
  * Recall the fundamental equation of accounting:
  *
  *  `Debits` === `Credits`
  *  `Assets` + `Expenses` === `Liabilities` + `Equity` + `Revenues`
  *
  *  Balance(Assets, LOQs)
  *  Balance(XOP, Revenues)  // !!!
  *
  * Both type params are needed to deal with case where MA =!= Q in the cake
  * enabling the package to create
  *   Money[MA, C] <=> (MI, Q)
  * codecs via MonetaryInstruments table.
  *
  * TODO: this design is provisional
  *
  * Note the explicit binding to `Ledger` with `Folio.Key` dependence.
  * FIXME: really only depends on `Ledger` - could merge but would need 2 type params
  */
abstract class Balances[MA: Financial, Q: Financial] extends EntityAccountMapping[Q] {

  type AccountType = keys.AccountType

  type Debit  = keys.Debit
  type Credit = keys.Credit

  type XOP     = keys.XOP
  type Revenue = keys.Revenue

  type Asset = keys.Asset
  type LOQ   = keys.LOQ

  type Expense = keys.Expense
  // type Profit = keys.Profit
  type Liability = keys.Liability
  type Equity    = keys.Equity

  /**
    * FIXME: depends on mapping between `Folio`s and `Balance`s
    */
  type EntryKey = (AccountType, Folio.Key)

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  type MonetaryAmount = MA
  val MonetaryAmount = Financial[MonetaryAmount]

  private final type AccountMap[A <: AccountType, CCY] = Map[A, Money[MA, CCY]]
  private object AccountMap {
    def empty[A <: AccountType, CCY: Currency]: AccountMap[A, CCY] = Map.empty[A, Money[MA, CCY]]
  }

  /** convenience and domain semantics only */
  def openAccount[A <: AccountType, CCY: Currency]: AccountMap[A, CCY] = AccountMap.empty

  final type Debits[CCY]      = AccountMap[Debit, CCY]
  final type Credits[CCY]     = AccountMap[Credit, CCY]
  final type XOPs[CCY]        = AccountMap[XOP, CCY]
  final type Revenues[CCY]    = AccountMap[Revenue, CCY]
  final type Assets[CCY]      = AccountMap[Asset, CCY]
  final type LOQs[CCY]        = AccountMap[LOQ, CCY]
  final type Expenses[CCY]    = AccountMap[Expense, CCY]
  final type Liabilities[CCY] = AccountMap[Liability, CCY]
  final type Equities[CCY]    = AccountMap[Equity, CCY]

  /** pure trait for the enum base */
  private[model] sealed trait BalanceLike extends Product with Serializable {
    type DebitType <: Debit
    type CreditType <: Credit
    type CurrencyType
    def currency(implicit CCY: Currency[CurrencyType]): Currency[CurrencyType] = CCY
    def ds[CCY]: AccountMap[DebitType, CCY]
    def cs[CCY]: AccountMap[CreditType, CCY]
  }

  /** specify key types */
  sealed abstract class Balance[D <: Debit, C <: Credit, CCY] private[Balances] (
      _ds: AccountMap[D, CCY],
      _cs: AccountMap[C, CCY]
  ) extends BalanceLike {

    final type DebitType  = D
    final type CreditType = C

    final type CurrencyType = CCY

    /** FIXME implementing this will take creatime downcasting... */
    final def ds[CCY]: AccountMap[DebitType, CCY]  = ??? // _ds
    final def cs[CCY]: AccountMap[CreditType, CCY] = ??? // _cs

    final def net(implicit CCY: Currency[CCY]): Money[MA, CCY] = ??? // FIXME cs.sumValues - ds.sumValues

  }

  /** */
  object Balance {
    def unapply[D <: Debit, C <: Credit, CCY: Currency](
        b: Balance[D, C, CCY]
    ): Option[(AccountMap[D, CCY], AccountMap[C, CCY])] = ???
    // (b.ds, b.cs).some

    def collect[T <: AccountType, R <: T: scala.reflect.ClassTag, CCY](
        as: AccountMap[T, CCY]
    ): AccountMap[R, CCY] =
      as collect {
        case (k: R, v) => (k, v)
      }

  }

  case class TrialBalance[CCY] private (
      debits: Debits[CCY],
      credits: Credits[CCY]
  ) extends Balance[Debit, Credit, CCY](debits, credits) {

    import Balance.collect

    def partition: (IncomeStatement[CCY], BalanceSheet[CCY]) =
      (
        IncomeStatement(debits |> collect, credits |> collect),
        BalanceSheet(debits    |> collect, credits |> collect)
      )

    def updated(uk: UpdateKey, amt: Money[MA, CCY]): TrialBalance[CCY] =
      copy(debits + (uk.debit -> amt), credits + (uk.credit -> amt))

    def swapped[T <: AccountType](sk: SwapKey[T], amt: Money[MA, CCY]): TrialBalance[CCY] =
      sk match {
        case AssetSwapKey(d1, d2) => copy(debits = debits + (d1   -> amt) + (d2 -> -amt))
        case LOQSwapKey(c1, c2)   => copy(credits = credits + (c1 -> amt) + (c2 -> -amt))
      }
  }
  object TrialBalance {
    implicit def trialBalanceCommutativeGroup[CCY: Currency]: CommutativeGroup[TrialBalance[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Debits[CCY], Credits[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /**
    * Note: These are a mixture of cash and accrual items when "raw".
    * A cash account can be determined from its `Instrument.Key`.
    * This can be used to create a filter for `CashFlowStatement`s.
    */
  final case class IncomeStatement[CCY] private (
      val xops: XOPs[CCY],
      val revenues: Revenues[CCY]
  ) extends Balance(xops, revenues) {

    def partition(implicit ci: CashInstruments[CCY]): (IncomeStatement[CCY], CashFlowStatement[CCY]) =
      ???
  }

  /**
    *
    */
  object IncomeStatement {
    implicit def incomeStatementCommutativeGroup[CCY: Currency]: CommutativeGroup[IncomeStatement[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(XOPs[CCY], Revenues[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /**
    * - `CashFlowStatement` is a Balance(debit, credit) like the IncomeStatement.
    * - Follow the `Money`. Operations, Investment, Financing
    * - TODO: cash keys should get their own flavors; for now reuse `Revenue` and `Expense`.
    */
  final case class CashFlowStatement[CCY] private (
      val outflows: XOPs[CCY],
      val inflows: Revenues[CCY]
  ) extends Balance(outflows, inflows)

  /**  */
  object CashFlowStatement {
    implicit def cashflowStatementCommutativeGroup[CCY: Currency]: CommutativeGroup[CashFlowStatement[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(XOPs[CCY], Revenues[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /** outputs: YearBegin, YearEnd, NetIncrease = YearEnd - YearBegin */
  }

  final case class EquityStatement[CCY] private (wut: Null)

  /**
    * `BalanceSheet`s form a `CommutativeGroup`.
    *  All operations are double entry by construction.
    */
  final case class BalanceSheet[CCY] private (
      val assets: Assets[CCY],
      val loqs: LOQs[CCY]
  ) extends Balance(assets, loqs)

  /** tax free implicits */
  object BalanceSheet {
    implicit def balanceCommutativeGroup[CCY: Currency]: CommutativeGroup[BalanceSheet[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Assets[CCY], LOQs[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  def trialBalance[F[_]: Foldable: Monad: MonoidK, CCY: Currency](ts: Transactions[F]): TrialBalance[CCY] =
    ???

  def breakdown[CCY: Currency: CashInstruments](
      prior: BalanceSheet[CCY],
      delta: BalanceSheet[CCY], // delta and raw come from TrialBalance
      raw: IncomeStatement[CCY] // mixed cash and accrual
  ): (CashFlowStatement[CCY], EquityStatement[CCY]) =
    ???
}
