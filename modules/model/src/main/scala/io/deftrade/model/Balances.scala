package io.deftrade
package model

import time._
import time.implicits._

import money._
import Currency.USD

import opaqueid._
import OpaqueId.Fresh

import repos._

import cats.{ Eq, Foldable, Hash, Invariant, Monad, Monoid, MonoidK, Order }
import cats.kernel.CommutativeGroup
import cats.data.Kleisli
import cats.implicits._
import feralcats.instances._

import eu.timepit.refined
import refined.{ cats => refinedCats, _ }
import refined.api.Refined
import refined.numeric._
import refined.string._
import refined.auto._

import io.circe.Json

import scala.language.higherKinds

/**
  * `Balance` calculation from a sequence of `Transaction`s.
  *
  * Recall the fundamental equation of accounting:
  *
  *  `Debit` === `Credit`
  *  `Assets` + `Expenses` === `Liabilities` + `Equity` + `Revenues`
  *
  *  Balance(Assets, LOQs)
  *  Balance(XOP, Revenues)  // !!!
  *
  * TODO: fix this comment – design is provisional
  * Both type params are needed to deal with case where MA =!= Q in the cake
  * this allows this package to deal with MA <=> Q functions via Monetary Instruments table.
  * Also binding explicitly to `Ledger` with `Folio.Id` dependence.
  * FIXME: really only depends on `Ledger`
  */
abstract class Balances[MA: Financial, Q: Financial] extends EntityAccountMapping[Q] {

  // type AccountType
  //
  // type Debit  <: AccountType
  // type Credit <: AccountType
  //
  // type XOP <: Debit
  // type Revenue <: Credit
  //
  // type Asset <: Debit
  // type LOQ <: Credit
  //
  // type Expense <: XOP
  // // type Profit <: XOP
  //
  // type Liability <: LOQ
  // type Equity <: LOQ

  type AccountType = enums.AccountType
  type Debit       = enums.Debit
  type Credit      = enums.Credit
  type XOP         = enums.XOP
  type LOQ         = enums.LOQ
  type Asset       = enums.Asset
  type Expense     = enums.Expense
  type Liability   = enums.Liability
  type Equity      = enums.Equity
  type Revenue     = enums.Revenue
  // type Profit = enums.Profit

  /**
    * FIXME: depends on mapping between `Folio`s and `Balance`s
    */
  type EntryKey = (AccountType, Folio.Id)

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  type MonetaryAmount = MA
  val MonetaryAmount = Financial[MonetaryAmount]
  import MonetaryAmount.{ fractional => MAF, commutativeGroup => MACG }

  private final type AccountMap[A <: AccountType] = Map[A, MonetaryAmount]
  private object AccountMap {
    def empty[A <: AccountType]: AccountMap[A] = Map.empty[A, MonetaryAmount]
  }

  final type Debits  = AccountMap[Debit]
  final type Credits = AccountMap[Credit]

  final type XOPs = AccountMap[XOP]
  final type LOQs = AccountMap[LOQ]

  final type Assets      = AccountMap[Asset]
  final type Expenses    = AccountMap[Expense]
  final type Liabilities = AccountMap[Liability]
  final type Equities    = AccountMap[Equity]
  final type Revenues    = AccountMap[Revenue]

  sealed abstract class Balance[D <: Debit, C <: Credit](val ds: AccountMap[D], val cs: AccountMap[C]) extends Product with Serializable
  object Balance

  final case class TrialBalance private (
      override val ds: Debits,
      override val cs: Credits
  ) extends Balance(ds, cs) {

    lazy val partition: (IncomeStatement, BalanceSheet) = {

      def collect[T <: AccountType, R <: T](as: AccountMap[T]): AccountMap[R] = as collect {
        case (k: R, v) => (k, v)
      }
      val assets: AccountMap[Asset] = collect(ds)
      val loqs: AccountMap[LOQ]     = collect(cs)

      val xops: AccountMap[XOP]         = collect(ds)
      val revenues: AccountMap[Revenue] = collect(cs)

      (IncomeStatement(xops, revenues), BalanceSheet(assets, loqs))
    }

    def updated(uk: UpdateKey, amt: MonetaryAmount): TrialBalance =
      copy(ds + (uk.debit -> amt), cs + (uk.credit -> amt))

    def swapped[T <: AccountType](sk: SwapKey[T], amt: MonetaryAmount): TrialBalance = sk match {
      case AssetSwapKey(d1, d2) => copy(ds = ds + (d1 -> amt) + (d2 -> amt.inverse))
      case LOQSwapKey(c1, c2)   => copy(cs = cs + (c1 -> amt) + (c2 -> amt.inverse))
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
    * A cash account can be determined from its `Instrument.Id`.
    * This can be used to create a filter for `CashFlowStatement`s.
    */
  final case class IncomeStatement private (
      val xops: XOPs,
      val revenues: Revenues
  ) extends Balance(xops, revenues) {
    def partition[C](implicit ci: CashInstruments): (IncomeStatement, IncomeStatement) = ???
  }

  /** */
  object IncomeStatement {
    implicit lazy val incomeStatementCommutativeGroup: CommutativeGroup[IncomeStatement] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(XOPs, Revenues)]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  final case class CashFlowStatement private (wut: Nothing)
  final case class EquityStatement private (wut: Nothing)

  /**
    * `BalanceSheet` forms a commutative group
    *   all balance sheet ops are double entry
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

  def trialBalance[F[_]: Foldable: Monad: MonoidK](jes: Journal[F, Journal.Entry]): TrialBalance =
    ???

  def breakdown(
      prior: BalanceSheet,
      delta: BalanceSheet, // delta and raw come from TrialBalance
      raw: IncomeStatement // mixed cash and accrual
  )(implicit ci: CashInstruments): (CashFlowStatement, EquityStatement) =
    ???
}
