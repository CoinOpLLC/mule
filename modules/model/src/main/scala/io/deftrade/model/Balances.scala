package io.deftrade
package model

import money._

import enums.{ AssetSwapKey, LOQSwapKey, SwapKey, UpdateKey }

import cats.{ Foldable, Invariant, Monad, MonoidK }
import cats.kernel.CommutativeGroup
import cats.implicits._
import feralcats.instances._

import eu.timepit.refined
import refined.auto._

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
import spire.math.Fractional
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

  type AccountType = enums.AccountType

  type Debit  = enums.Debit
  type Credit = enums.Credit

  type XOP     = enums.XOP
  type Revenue = enums.Revenue

  type Asset = enums.Asset
  type LOQ   = enums.LOQ

  type Expense = enums.Expense
  // type Profit = enums.Profit
  type Liability = enums.Liability
  type Equity    = enums.Equity

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
      debits: AccountMap[D],
      credits: AccountMap[C]
  ) extends BalanceLike {
    final type DebitType  = D
    final type CreditType = C
    final def ds = debits
    final def cs = credits
  }

  /** */
  object Balance {
    def unapply[D <: Debit, C <: Credit](
        b: Balance[D, C]
    ): Option[(AccountMap[D], AccountMap[C])] =
      (b.ds, b.cs).some
  }

  case class TrialBalance private (
      debits: Debits,
      credits: Credits
  ) extends Balance(debits, credits) {

    lazy val partition: (IncomeStatement, BalanceSheet) = {
      import scala.reflect.ClassTag
      def collect[T <: AccountType, R <: T: ClassTag](as: AccountMap[T]): AccountMap[R] =
        as collect {
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
      case AssetSwapKey(d1, d2) => copy(debits = ds + (d1  -> amt) + (d2 -> -amt))
      case LOQSwapKey(c1, c2)   => copy(credits = cs + (c1 -> amt) + (c2 -> -amt))
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
    * - TODO: cash keys should get their own flavors
    */
  final case class CashFlowStatement private (
      val outflows: XOPs,
      val inflows: Revenues
  ) extends Balance(outflows, inflows) {

    def net: MA = sumOf(inflows) - sumOf(outflows)
  }

  /** Follow the `Money`. */
  object CashFlowStatement {

    /** operations, investment, financing */

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
