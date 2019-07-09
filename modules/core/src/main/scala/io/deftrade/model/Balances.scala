package io.deftrade
package model

import money._
import time._

import keys.{ AssetSwapKey, CreditKey, DebitKey, LiabilitySwapKey, SwapKey }

import cats.{ Foldable, Invariant, Monad, Reducible, SemigroupK }
import cats.data.NonEmptyList
import cats.kernel.CommutativeGroup
import feralcats.instances._
import cats.implicits._

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
  *  Balance(XOI, Revenues)
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

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  type MonetaryAmount = MA
  val MonetaryAmount = Financial[MonetaryAmount]

  type PricedTrade[C] = (Trade, Money[MonetaryAmount, C])

  object PricedTrade {
    def apply[C: Currency: Wallet](pt: PricedTrade[C]): Trade = PricedTrade.normalize(pt)

    /**
      * Used to convert to the currency as `Instrument` convention.
      */
    def normalize[C: Currency](pt: PricedTrade[C])(implicit ci: Wallet[C]): Trade = ???
  }

  /** type alias */
  type ValuedFolio[C] = PricedTrade[C]

  lazy val ValuedFolio = PricedTrade

  final case class TransactionMarker[CCY](
      mark: Trade => PricedTrade[CCY]
  )

  type AccountType = keys.AccountType

  type Debit  = keys.Debit
  type Credit = keys.Credit

  type Expense = keys.Expense
  type Revenue = keys.Revenue

  type Asset     = keys.Asset
  type Liability = keys.Liability

  type Income = keys.Income
  type Equity = keys.Equity

  /** A tally sheet, basically. Partially specialized `Map` with special tricks. */
  final type AccountMap[A <: AccountType, CCY] = Map[A, Money[MA, CCY]]

  /* */
  object AccountMap {

    def empty[AT <: AccountType, CCY]: AccountMap[AT, CCY] = Map.empty[AT, Money[MA, CCY]]

    def from[AT <: AccountType, CCY](
        ks: keys.SwapKey[AT],
        amount: Money[MA, CCY]
    ): AccountMap[AT, CCY] = ???

    def collect[T <: AccountType, R <: T: scala.reflect.ClassTag, CCY](
        as: AccountMap[T, CCY]
    ): AccountMap[R, CCY] =
      as collect {
        case (k: R, v) => (k, v)
      }
  }

  implicit class SweetAccountMap[A <: AccountType, CCY](am: AccountMap[A, CCY]) {

    def total(implicit CCY: Currency[CCY]): Money[MA, CCY] =
      CCY(NonEmptyList(Fractional[MA].zero, am.values.toList.map(_.amount)).reduce)

  }

  /** convenience and domain semantics only */
  def emptyAccount[A <: AccountType, CCY: Currency]: AccountMap[A, CCY] = AccountMap.empty

  final type Debits[CCY]      = AccountMap[Debit, CCY]
  final type Credits[CCY]     = AccountMap[Credit, CCY]
  final type Revenues[CCY]    = AccountMap[Revenue, CCY]
  final type Assets[CCY]      = AccountMap[Asset, CCY]
  final type Expenses[CCY]    = AccountMap[Expense, CCY]
  final type Liabilities[CCY] = AccountMap[Liability, CCY]
  final type Incomes[CCY]     = AccountMap[Income, CCY]
  final type Equities[CCY]    = AccountMap[Equity, CCY]

  /** pure trait for the enum base */
  private[model] sealed trait BalanceLike extends Product with Serializable {

    type CurrencyType
    def currency(implicit CCY: Currency[CurrencyType]): Currency[CurrencyType] = CCY

    type DebitType <: Debit
    def ds: AccountMap[DebitType, CurrencyType]

    type CreditType <: Credit
    def cs: AccountMap[CreditType, CurrencyType]

  }

  /** specify key types */
  sealed abstract class Balance[D <: Debit, C <: Credit, CCY] private[Balances] (
      _ds: AccountMap[D, CCY],
      _cs: AccountMap[C, CCY]
  ) extends BalanceLike {

    final type DebitType  = D
    final type CreditType = C

    final type CurrencyType = CCY

    final def ds: AccountMap[DebitType, CurrencyType]  = _ds
    final def cs: AccountMap[CreditType, CurrencyType] = _cs

    /**
      * n.b. as it stands, this is useful as a check only, because the algebra is balanced by design
      */
    final def net(implicit CCY: Currency[CCY]): Money[MA, CCY] = cs.total - ds.total

  }

  /** */
  object Balance {

    def unapply[D <: Debit, C <: Credit, CCY: Currency](
        b: Balance[D, C, CCY]
    ): Option[(AccountMap[D, CCY], AccountMap[C, CCY])] = (b.ds, b.cs).some
  }

  /** */
  final case class TrialBalance[CCY] private (
      debits: Debits[CCY],
      credits: Credits[CCY]
  ) extends Balance[Debit, Credit, CCY](debits, credits) {

    import AccountMap.collect

    def updated(key: DebitKey, amount: Money[MA, CCY]): TrialBalance[CCY]  = ???
    def updated(key: CreditKey, amount: Money[MA, CCY]): TrialBalance[CCY] = ???

    def swapped[T <: AccountType](sk: SwapKey[T], amt: Money[MA, CCY]): TrialBalance[CCY] = {

      def CG[AT <: AccountType] = CommutativeGroup[AccountMap[AT, CCY]]

      sk match {
        // FIXME: make syntax work
        case ks @ AssetSwapKey(_, _) => ???
        // case ks @ LiabilitySwapKey(_, _)   => ???
      }
    }

    def partition: (IncomeStatement[CCY], BalanceSheet[CCY]) =
      (
        IncomeStatement(debits |> collect, credits |> collect),
        BalanceSheet(debits    |> collect, credits |> collect)
      )
  }

  /** */
  object TrialBalance {

    def empty[CCY: Currency]: TrialBalance[CCY] =
      TrialBalance(
        debits = emptyAccount[Debit, CCY],
        credits = emptyAccount[Credit, CCY]
      )

    implicit def trialBalanceCommutativeGroup[CCY: Currency]: CommutativeGroup[TrialBalance[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Debits[CCY], Credits[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    def from[CC[_]: Foldable, CCY: Currency](
        marker: TransactionMarker[CCY]
    )(
        xs: CC[Transaction]
    ): TrialBalance[CCY] = ??? // xs.sum // FIXME this is all I should have to say!

  }

  /**
    * Note: These are a mixture of cash and accrual items when "raw".
    * A cash account can be determined from its `Instrument.Key`.
    * This can be used to create a filter for `CashFlowStatement`s.
    */
  final case class IncomeStatement[CCY] private (
      val expenses: Expenses[CCY],
      val revenues: Revenues[CCY]
  ) extends Balance(expenses, revenues) {

    def partition(implicit ci: Wallet[CCY]): (IncomeStatement[CCY], CashFlowStatement[CCY]) =
      ???
  }

  /**
    *
    */
  object IncomeStatement {
    implicit def incomeStatementCommutativeGroup[CCY: Currency]: CommutativeGroup[IncomeStatement[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Expenses[CCY], Revenues[CCY])]) {
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
      val outflows: Expenses[CCY],
      val inflows: Revenues[CCY]
  ) extends Balance(outflows, inflows)

  /**  */
  object CashFlowStatement {
    implicit def cashflowStatementCommutativeGroup[CCY: Currency]: CommutativeGroup[CashFlowStatement[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Expenses[CCY], Revenues[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /** outputs: YearBegin, YearEnd, NetIncrease = YearEnd - YearBegin */
  }

  final case class EquityStatement[CCY] private (wut: Null)
  object EquityStatement

  /**
    * `BalanceSheet`s form a `CommutativeGroup`.
    *  All operations are double entry by construction.
    */
  final case class BalanceSheet[CCY] private (
      val assets: Assets[CCY],
      val liabilities: Liabilities[CCY]
  ) extends Balance(assets, liabilities)

  /** tax free implicits */
  object BalanceSheet {
    implicit def balanceCommutativeGroup[CCY: Currency]: CommutativeGroup[BalanceSheet[CCY]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Assets[CCY], Liabilities[CCY])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  sealed trait CoversPeriod {
    def date: LocalDate
    def period: Period
    final def startDate = date - period
  }

  final case class CashBookSet[C: Currency](
      date: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      bs: BalanceSheet[C]
  ) extends CoversPeriod {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  final case class AccrualBookSet[C: Currency](
      date: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      is: IncomeStatement[C],
      bs: BalanceSheet[C]
  ) extends CoversPeriod {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  def trialBalance[F[_]: Foldable: Monad: SemigroupK, CCY: Currency](
      ts: F[Transaction]
  ): TrialBalance[CCY] = ???

  def breakdown[CCY: Currency: Wallet](
      prior: BalanceSheet[CCY],
      delta: BalanceSheet[CCY], // delta and raw come from TrialBalance
      raw: IncomeStatement[CCY] // mixed cash and accrual
  ): (CashFlowStatement[CCY], EquityStatement[CCY]) =
    ???

  type DeltaCashBooks[CCY]    = (CashFlowStatement[CCY], BalanceSheet[CCY])
  type DeltaAccrualBooks[CCY] = (IncomeStatement[CCY], CashFlowStatement[CCY], BalanceSheet[CCY])

  /**
    * Cratchit needs to look at the current state of the books
    * in order to properly allocate balance sheet items (in the most general case)
    */
  def deltaCashBooksFrom[CCY: Currency](cbs: CashBookSet[CCY]): (
      PricedTrade[CCY],
      UnitPartition[AccountType, MonetaryAmount],
      Transaction.Meta
  ) => DeltaCashBooks[CCY] = ???

  import enumeratum._

  sealed trait NettableLike extends EnumEntry with Product with Serializable {
    type AssetType <: Debit
    def gross: AssetType
    def less: AssetType
  }

  abstract class Nettable[D <: Asset](
      val gross: D,
      val less: D
  ) extends NettableLike {

    final type AssetType = D

    final def net[C <: Credit, CCY: Currency](b: Balance[D, C, CCY]): Money[MA, CCY] = b match {
      case Balance(ds, _) => ds(gross) - ds(less)
    }
    // b.ds(gross) - b.ds(less) // TODO: this is typesafe, but not fool-proof.
  }

  /** FIXME this can move outside of Cake */
  object Nettable extends Enum[NettableLike] {

    import keys.Asset._

    case object Depreciable
        extends Nettable(
          BuildingsAndOtherDepreciableAssets,
          LessAccumulatedDepreciation
        )

    case object Depletable
        extends Nettable(
          DepletableAssets,
          LessAccumulatedDepletion
        )
    case object Amortizable
        extends Nettable(
          IntangibleAssets,
          LessAccumulatedAmortization
        )

    val values = findValues

  }
}
