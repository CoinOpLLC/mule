package io.deftrade
package model

import money._, time._, keyval._

import accounting.{ AssetSwapKey, CreditKey, DebitKey, SwapKey }

import cats.{ Foldable, Invariant, Monad, SemigroupK }
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

  type Pricer[C] = Trade => PricedTrade[C]
  sealed abstract case class TransactionMarker[C](
      mark: Pricer[C]
  )
  object TransactionMarker extends WithOpaqueKey[Long, TransactionMarker[Currency.USD]] {
    def apply[C: Currency](mark: Pricer[C]): TransactionMarker[C] = new TransactionMarker(mark) {}
  }

  type AccountType = accounting.AccountType

  type Debit  = accounting.Debit
  type Credit = accounting.Credit

  type Expense = accounting.Expense
  type Revenue = accounting.Revenue

  type Asset     = accounting.Asset
  type Liability = accounting.Liability

  type Income = accounting.Income
  type Equity = accounting.Equity

  /** A tally sheet, basically. Partially specialized `Map` with special tricks. */
  final type AccountMap[A <: AccountType, C] = Map[A, Money[MA, C]]

  /* */
  object AccountMap {

    def empty[AT <: AccountType, C]: AccountMap[AT, C] = Map.empty[AT, Money[MA, C]]

    def from[AT <: AccountType, C](
        ks: accounting.SwapKey[AT],
        amount: Money[MA, C]
    ): AccountMap[AT, C] = ???

    def collect[T <: AccountType, R <: T: scala.reflect.ClassTag, C](
        as: AccountMap[T, C]
    ): AccountMap[R, C] =
      as collect {
        case (k: R, v) => (k, v)
      }
  }

  implicit class SweetAccountMap[A <: AccountType, C](am: AccountMap[A, C]) {

    def total(implicit C: Currency[C]): Money[MA, C] =
      C(NonEmptyList(Fractional[MA].zero, am.values.toList.map(_.amount)).reduce)

  }

  /** convenience and domain semantics only */
  def emptyAccount[A <: AccountType, C: Currency]: AccountMap[A, C] = AccountMap.empty

  final type Debits[C]      = AccountMap[Debit, C]
  final type Credits[C]     = AccountMap[Credit, C]
  final type Revenues[C]    = AccountMap[Revenue, C]
  final type Assets[C]      = AccountMap[Asset, C]
  final type Expenses[C]    = AccountMap[Expense, C]
  final type Liabilities[C] = AccountMap[Liability, C]
  final type Incomes[C]     = AccountMap[Income, C]
  final type Equities[C]    = AccountMap[Equity, C]

  /** pure trait for the enum base */
  private[model] sealed trait BalanceLike extends Product with Serializable {

    type CurrencyType
    def currency(implicit C: Currency[CurrencyType]): Currency[CurrencyType] = C

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
  sealed abstract case class TrialBalance[C] private (
      debits: Debits[C],
      credits: Credits[C]
  ) extends Balance[Debit, Credit, C](debits, credits) {

    // FIXME import AccountMap.collect

    def updated(key: DebitKey, amount: Money[MA, C]): TrialBalance[C]  = ???
    def updated(key: CreditKey, amount: Money[MA, C]): TrialBalance[C] = ???

    // implicit def CG[AT <: AccountType] = CommutativeGroup[AccountMap[AT, C]]
    def swapped[T <: AccountType](sk: SwapKey[T], amt: Money[MA, C]): TrialBalance[C] =
      sk match {
        // FIXME: make syntax work
        case AssetSwapKey(_, _) =>
          ???
        // case ks @ LiabilitySwapKey(_, _)   => ???
      }

    def partition: (IncomeStatement[C], BalanceSheet[C]) = ???
    // FIXME `Any` problems
    // (
    //   IncomeStatement(debits |> collect, credits |> collect),
    //   BalanceSheet(debits    |> collect, credits |> collect)
    // )
  }

  /** */
  object TrialBalance {

    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): TrialBalance[C] = new TrialBalance(debits, credits) {}

    def empty[C: Currency]: TrialBalance[C] =
      apply(
        debits = emptyAccount[Debit, C],
        credits = emptyAccount[Credit, C]
      )

    implicit def trialBalanceCommutativeGroup[C: Currency]: CommutativeGroup[TrialBalance[C]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Debits[C], Credits[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    def from[CC[_]: Foldable, C: Currency](
        marker: TransactionMarker[C]
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

    def partition(implicit ci: Wallet[C]): (IncomeStatement[C], CashFlowStatement[C]) =
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
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Expenses[C], Revenues[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /**
    * - `CashFlowStatement` is a Balance(debit, credit) like the IncomeStatement.
    * - Follow the `Money`. Operations, Investment, Financing
    * - TODO: cash accounting should get their own flavors; for now reuse `Revenue` and `Expense`.
    */
  sealed abstract case class CashFlowStatement[C] private (
      val outflows: Expenses[C],
      val inflows: Revenues[C]
  ) extends Balance(outflows, inflows)

  /**  */
  object CashFlowStatement {

    private[model] def apply[C: Currency](
        outflows: Expenses[C],
        inflows: Revenues[C]
    ): CashFlowStatement[C] = new CashFlowStatement(outflows, inflows) {}

    implicit def cashflowStatementCommutativeGroup[C: Currency]: CommutativeGroup[CashFlowStatement[C]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Expenses[C], Revenues[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /** outputs: YearBegin, YearEnd, NetIncrease = YearEnd - YearBegin */
  }

  sealed abstract case class EquityStatement[C] private (wut: Null)
  object EquityStatement

  /**
    * `BalanceSheet`s form a `CommutativeGroup`.
    *  All operations are double entry by construction.
    */
  sealed abstract case class BalanceSheet[C] private (
      val assets: Assets[C],
      val liabilities: Liabilities[C]
  ) extends Balance(assets, liabilities)

  /** tax free implicits */
  object BalanceSheet {

    private[model] def apply[C: Currency](
        assets: Assets[C],
        liabilities: Liabilities[C]
    ): BalanceSheet[C] = new BalanceSheet(assets, liabilities) {}

    implicit def balanceCommutativeGroup[C: Currency]: CommutativeGroup[BalanceSheet[C]] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Assets[C], Liabilities[C])]) {
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

  sealed abstract case class CashBookSet[C: Currency](
      date: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      bs: BalanceSheet[C]
  ) extends CoversPeriod {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  sealed abstract case class AccrualBookSet[C: Currency](
      date: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      is: IncomeStatement[C],
      bs: BalanceSheet[C]
  ) extends CoversPeriod {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  def trialBalance[F[_]: Foldable: Monad: SemigroupK, C: Currency](
      ts: F[Transaction]
  ): TrialBalance[C] = ???

  def breakdown[C: Currency: Wallet](
      prior: BalanceSheet[C],
      delta: BalanceSheet[C], // delta and raw come from TrialBalance
      raw: IncomeStatement[C] // mixed cash and accrual
  ): (CashFlowStatement[C], EquityStatement[C]) =
    ???

  type DeltaCashBooks[C]    = (CashFlowStatement[C], BalanceSheet[C])
  type DeltaAccrualBooks[C] = (IncomeStatement[C], CashFlowStatement[C], BalanceSheet[C])

  /**
    * Cratchit needs to look at the current state of the books
    * in order to properly allocate balance sheet items (in the most general case)
    */
  def deltaCashBooksFrom[C: Currency](cbs: CashBookSet[C]): (
      PricedTrade[C],
      UnitPartition[AccountType, MonetaryAmount],
      Transaction.Meta
  ) => DeltaCashBooks[C] = ???

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

    import accounting.Asset._

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
