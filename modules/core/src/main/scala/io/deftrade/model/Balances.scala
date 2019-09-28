package io.deftrade
package model

import money._, time._, keyval._, accounting._

import cats.implicits._
import cats.{ Foldable, Invariant, Monad, SemigroupK }
import cats.kernel.CommutativeGroup
import feralcats.instances._

import eu.timepit.refined
import refined.auto._

// narrow import to get `Field` operators (only!) for `Fractional` spire types.
import spire.syntax.field._

import scala.language.higherKinds

/**
  * Double entry [[Balance]] calculation from a sequence of [[Transaction]]s.
  *
  * Recall the fundamental equation of double entry bookkeeping:
  *
  *   - `Debits === Credits`
  *
  * Expanding:
  *
  *   - `Assets + Expenses === Liabilities + Equity + Revenues`
  *
  * When summing Transactions, this "cake slice" module implements the algebra which
  * maintains the above equality.
  *
  * Note both type params `MA` and `Q` are needed to deal with case
  * where [[MonetaryAmount]] and [[Quantity]]
  * are distinct types (e.g. [[scala.BigDecimal]] and [[scala.Double]], respectively.)
  *
  * Modules parameterized like this may create `Money[MA, C] <=> (MI, Q)` codecs via a table of
  * [[capital.Instrument]]s which function as stable, denominated currency (e.g. a bank account, or
  * a money market fund instrument.)
  *
  */
abstract class Balances[MA: Financial, Q: Financial] extends Pricing[MA, Q] {

  /** instantiate double entry key module with appropriate monetary amount type */
  object doubleEntryKeys extends DoubleEntryKeys[MonetaryAmount]
  import doubleEntryKeys._

  /** Mapping accounting keys to [[money.Money]]. */
  final type AccountMap[A <: AccountingKey, C] = Map[A, Mny[C]]

  /** */
  object AccountMap {

    /** */
    def fromSwapKey[A <: AccountingKey, C: Currency](
        ks: SwapKey[A],
        amount: Mny[C]
    ): AccountMap[A, C] = {
      def from = ks.from.toSortedMap mapValues (-amount * _)
      def to   = ks.to.toSortedMap mapValues (amount * _)
      from |+| to
    }

    implicit final class Ops[K <: AccountingKey, C: Currency](am: AccountMap[K, C]) {

      /**
        * Filters a map by narrowing the scope of the keys contained.
        *
        * TODO: Revisit. This is awkward, but DRY and reflection free... needs to evolve.
        *
        * @param subKey Easily provided via an extractor.
        * @return A map containing those entries whose keys match a subclassing pattern.
        * @see [[keyval.DtEnum]]
        *
        */
      def collectKeys[L <: K](subKey: K => Option[L]): AccountMap[L, C] =
        am collect (Function unlift { case (k, v) => subKey(k) map (l => (l, v)) })

      /** TODO: sketchy... de-sketchify */
      def widenKeys[J >: K <: AccountingKey]: AccountMap[J, C] =
        (am map widenKey[K, J, Mny[C]]).toMap

    }
    private def widenKey[K, J >: K, V]: ((K, V)) => (J, V) = identity

    /** */
    implicit final class MapOps[K <: AccountingKey, N: Financial](m: Map[K, N]) {

      /** */
      def denominated[C: Currency]: AccountMap[K, C] =
        m map {
          case (k, n) =>
            (k, Currency[C] fiat (Financial[N] to [MonetaryAmount] n))
        }
    }
  }
  import AccountMap._

  /** convenience and domain semantics only */
  def emptyAccount[A <: AccountingKey, C: Currency]: AccountMap[A, C] = Map.empty[A, Mny[C]]

  /** */
  type AccountingKey = accounting.AccountingKey

  type Debit  = accounting.Debit
  type Credit = accounting.Credit

  type Expense = accounting.Expense
  type Revenue = accounting.Revenue

  type Asset     = accounting.Asset
  type Liability = accounting.Liability

  type Income = accounting.Income
  type Equity = accounting.Equity

  /** */
  final type Debits[C] = AccountMap[Debit, C]

  /** */
  final type Credits[C] = AccountMap[Credit, C]

  /** [[BalanceSheet]] assets */
  final type Assets[C] = AccountMap[Asset, C]

  /** [[BalanceSheet]] liabilities */
  final type Liabilities[C] = AccountMap[Liability, C]

  /** Assets net of Liabilities */
  final type Equities[C] = AccountMap[Equity, C]

  /** "Top line" */
  final type Revenues[C] = AccountMap[Revenue, C]

  /** */
  final type Expenses[C] = AccountMap[Expense, C]

  /** Revenues net of Expenses */
  final type Incomes[C] = AccountMap[Income, C]

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

    import io.deftrade.implicits._ // FIXME: why tf does this have to be here?

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
    ): Option[(AccountMap[D, CCY], AccountMap[C, CCY])] = (b.debits, b.credits).some
  }

  /** Most general / least safe... */
  sealed abstract case class TrialBalance[C] private (
      override val debits: Debits[C],
      override val credits: Credits[C]
  ) extends Balance(debits, credits) {

    /** */
    def updated(dc: DebitCreditKey[Debit, Credit], amount: Mny[C])(
        implicit C: Currency[C]
    ): TrialBalance[C] =
      TrialBalance(debits |+| (dc.debits scaled -amount), credits |+| (dc.credits scaled amount))

    /** */
    def swapped[T <: AccountingKey](sk: SwapKey[T], amount: Mny[C])(
        implicit C: Currency[C]
    ): TrialBalance[C] =
      sk match {
        case AssetSwapKey(ask) =>
          TrialBalance(debits |+| AccountMap.fromSwapKey(ask, amount).widenKeys, credits)
        case LiabilitySwapKey(lsk) =>
          TrialBalance(debits, credits |+| AccountMap.fromSwapKey(lsk, amount).widenKeys)
      }

    /** */
    def partition: (IncomeStatement[C], BalanceSheet[C]) = ???
    // FIXME `Any` problems
    // (
    //   IncomeStatement(debits |> collect, credits |> collect),
    //   BalanceSheet(debits    |> collect, credits |> collect)
    // )
  }

  /** */
  object TrialBalance {

    /** */
    private[model] def apply[C: Currency](
        debits: Debits[C],
        credits: Credits[C]
    ): TrialBalance[C] = new TrialBalance(debits, credits) {}

    /** */
    def empty[C: Currency]: TrialBalance[C] =
      apply(
        debits = emptyAccount[Debit, C],
        credits = emptyAccount[Credit, C]
      )

    /** */
    implicit def trialBalanceCommutativeGroup[C: Currency]: CommutativeGroup[TrialBalance[C]] =
      (Invariant[CommutativeGroup] imap CommutativeGroup[(Debits[C], Credits[C])]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

    /** */
    def from[CC[_]: Foldable, C: Currency](
        marker: TradePricer[C]
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

    /** */
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
    * Follow the `Money`.
    * - Operations
    * - Investment
    * - Financing
    * - TODO: should cash accounting get their own flavors? for now reuse `Revenue` and `Expense`.
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
  sealed abstract case class EquityStatement[C] private (wut: Null)

  /** */
  object EquityStatement

  /**
    * `BalanceSheet`s form a `CommutativeGroup`.
    *  All operations are double entry by construction.
    */
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

  /** */
  sealed trait BookSet[C] {
    def date: LocalDate
    def period: Period
    final def startDate = date - period
    def cs: CashFlowStatement[C]
    def bs: BalanceSheet[C]
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): BookSet[C]
  }

  /** */
  sealed abstract case class CashBookSet[C](
      date: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      bs: BalanceSheet[C]
  ) extends BookSet[C] {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  /** */
  object CashBookSet {
    def apply[C: Currency](
        date: LocalDate,
        period: Period,
        cs: CashFlowStatement[C],
        bs: BalanceSheet[C]
    ): CashBookSet[C] = new CashBookSet(date, period, cs, bs) {}
  }

  /** */
  sealed abstract case class AccrualBookSet[C](
      date: LocalDate,
      period: Period,
      cs: CashFlowStatement[C],
      is: IncomeStatement[C],
      bs: BalanceSheet[C]
  ) extends BookSet[C] {
    def nextPeriod[L[_]: Foldable](xs: L[Transaction]): CashBookSet[C] = ???
  }

  /** */
  object AccrualBookSet {
    def apply[C: Currency](
        date: LocalDate,
        period: Period,
        cs: CashFlowStatement[C],
        is: IncomeStatement[C],
        bs: BalanceSheet[C]
    ): AccrualBookSet[C] = new AccrualBookSet(date, period, cs, is, bs) {}

  }

  /** */
  def trialBalance[F[_]: Foldable: Monad: SemigroupK, C: Currency](
      ts: F[Transaction]
  ): TrialBalance[C] = ???

  /** */
  def breakdown[C: Currency: Wallet](
      prior: BalanceSheet[C],
      delta: BalanceSheet[C], // delta and raw come from TrialBalance
      raw: IncomeStatement[C] // mixed cash and accrual
  ): (CashFlowStatement[C], EquityStatement[C]) =
    ???

  /**  */
  type DeltaCashBooks[C] = (CashFlowStatement[C], BalanceSheet[C])

  /** */
  type DeltaAccrualBooks[C] = (IncomeStatement[C], CashFlowStatement[C], BalanceSheet[C])

  /**
    * Cratchit needs to look at the current state of the books
    * in order to properly allocate balance sheet items (in the most general case)
    */
  def deltaCashBooksFrom[C: Currency](cbs: CashBookSet[C]): (
      PricedTrade[C],
      UnitPartition[AccountingKey, MonetaryAmount],
      Transaction.Meta
  ) => DeltaCashBooks[C] = ???

  import enumeratum._

  /** */
  sealed trait NettableLike extends EnumEntry with Serializable {
    type AssetType <: Debit
    def gross: AssetType
    def less: AssetType
  }

  /** */
  abstract class Nettable[D <: Asset](
      val gross: D,
      val less: D
  ) extends NettableLike {

    /** */
    final type AssetType = D

    /** */
    final def net[C <: Credit, CCY: Currency](b: Balance[D, C, CCY]): Money[MA, CCY] = b match {
      case Balance(ds, _) => ds(gross) - ds(less)
    }
    // b.ds(gross) - b.ds(less) // TODO: this is typesafe, but not fool-proof.
  }

  /**
    * Depreciation, depletion, and amortization are the reasons some [[Asset]]s are Nettable.
    *
    * TODO: Any other vocabularies to examine? Make this (differently) extensible?
    */
  object Nettable extends DtEnum[NettableLike] {

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
