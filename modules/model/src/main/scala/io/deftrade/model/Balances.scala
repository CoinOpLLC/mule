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

  private final type AccountMap[A <: AccountType] = Map[A, MonetaryAmount]
  private object AccountMap {
    def empty[A <: AccountType]: AccountMap[A] = Map.empty[A, MonetaryAmount]
  }

  /** convenience and domain semantics only */
  def openAccount[A <: AccountType]: AccountMap[A] = AccountMap.empty

  final type Debits  = AccountMap[Debit]
  final type Credits = AccountMap[Credit]

  final type XOPs     = AccountMap[XOP]
  final type Revenues = AccountMap[Revenue]

  final type Assets = AccountMap[Asset]
  final type LOQs   = AccountMap[LOQ]

  final type Expenses    = AccountMap[Expense]
  final type Liabilities = AccountMap[Liability]
  final type Equities    = AccountMap[Equity]

  /** pure trait for the enum base */
  private[model] sealed trait BalanceLike extends Product with Serializable {
    type DebitType <: Debit
    type CreditType <: Credit
    def ds: AccountMap[DebitType]
    def cs: AccountMap[CreditType]
  }

  /** specify key types */
  sealed abstract class Balance[D <: Debit, C <: Credit] private[Balances] (
      _ds: AccountMap[D],
      _cs: AccountMap[C]
  ) extends BalanceLike {

    final type DebitType  = D
    final type CreditType = C

    final def ds: AccountMap[DebitType]  = _ds
    final def cs: AccountMap[CreditType] = _cs

    final def net: MA = cs.sumValues - ds.sumValues

  }

  /** */
  object Balance {
    def unapply[D <: Debit, C <: Credit](
        b: Balance[D, C]
    ): Option[(AccountMap[D], AccountMap[C])] =
      (b.ds, b.cs).some

    def collect[T <: AccountType, R <: T: scala.reflect.ClassTag](
        as: AccountMap[T]
    ): AccountMap[R] =
      as collect {
        case (k: R, v) => (k, v)
      }

  }

  case class TrialBalance private (
      debits: Debits,
      credits: Credits
  ) extends Balance(debits, credits) {

    import Balance.collect

    def partition: (IncomeStatement, BalanceSheet) =
      (
        IncomeStatement(debits |> collect, credits |> collect),
        BalanceSheet(debits    |> collect, credits |> collect)
      )

    def updated(uk: UpdateKey, amt: MonetaryAmount): TrialBalance =
      copy(debits + (uk.debit -> amt), credits + (uk.credit -> amt))

    def swapped[T <: AccountType](sk: SwapKey[T], amt: MonetaryAmount): TrialBalance = sk match {
      case AssetSwapKey(d1, d2) => copy(debits = debits + (d1   -> amt) + (d2 -> -amt))
      case LOQSwapKey(c1, c2)   => copy(credits = credits + (c1 -> amt) + (c2 -> -amt))
    }
  }
  object TrialBalance {
    implicit lazy val trialBalanceCommutativeGroup: CommutativeGroup[TrialBalance] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Debits, Credits)]) {
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
  final case class IncomeStatement private (
      val xops: XOPs,
      val revenues: Revenues
  ) extends Balance(xops, revenues) {
    def partition[C](implicit ci: CashInstruments): (IncomeStatement, CashFlowStatement) = ???
  }

  /**
    *
    */
  object IncomeStatement {
    implicit lazy val incomeStatementCommutativeGroup: CommutativeGroup[IncomeStatement] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(XOPs, Revenues)]) {
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
  final case class CashFlowStatement private (
      val outflows: XOPs,
      val inflows: Revenues
  ) extends Balance(outflows, inflows)

  /**  */
  object CashFlowStatement {
    implicit lazy val incomeStatementCommutativeGroup: CommutativeGroup[CashFlowStatement] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(XOPs, Revenues)]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /** outputs: YearBegin, YearEnd, NetIncrease = YearEnd - YearBegin */
  }

  final case class EquityStatement private (wut: Null)

  /**
    * `BalanceSheet`s form a `CommutativeGroup`.
    *  All operations are double entry by construction.
    */
  final case class BalanceSheet private (
      val assets: Assets,
      val loqs: LOQs
  ) extends Balance(assets, loqs)

  /** tax free implicits */
  object BalanceSheet {
    implicit lazy val balanceCommutativeGroup: CommutativeGroup[BalanceSheet] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Assets, LOQs)]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  def trialBalance[F[_]: Foldable: Monad: MonoidK](ts: Transactions[F]): TrialBalance =
    ???

  def breakdown(
      prior: BalanceSheet,
      delta: BalanceSheet, // delta and raw come from TrialBalance
      raw: IncomeStatement // mixed cash and accrual
  )(implicit ci: CashInstruments): (CashFlowStatement, EquityStatement) =
    ???
}
