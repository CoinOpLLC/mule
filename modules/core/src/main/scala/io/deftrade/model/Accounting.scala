package io.deftrade
package model

import money.{ Currency, Financial }, keyval.DtEnum

import cats.implicits._

import enumeratum._

/**
  * Core accounting vocabulary.
  *
  * {{{Assets + Expenses = Liabilities + Equity + Income}}}
  *
  * TODO: abstracting across ontologies isn't going to be easy with this scheme,
  * because of the reliance on `sealed` hierarchies of enumerated values.
  */
trait Accounting { self: ModuleTypes =>

  /** */
  sealed trait AccountingKey extends EnumEntry with Product with Serializable

  /** */
  object AccountingKey {

    /** this is just a hack to use `SortedSet`s  */
    implicit def orderKeys[AT <: AccountingKey]: cats.Order[AT] = cats.Order by (_.entryName)
  }

  /** */
  sealed trait Debit extends AccountingKey
  object Debit extends DtEnum[Debit] {

    /** */
    lazy val values = Asset.values ++ Expense.values
  }

  /** */
  sealed trait Credit extends AccountingKey
  object Credit extends DtEnum[Credit] {

    /** */
    lazy val values = Liability.values ++ Equity.values ++ Revenue.values
  }

  /** */
  trait Expense extends Debit
  val Expense: DtEnum[_ <: Expense]

  /** */
  trait Revenue extends Credit
  val Revenue: DtEnum[_ <: Revenue]

  /** */
  trait Asset extends Debit
  val Asset: DtEnum[_ <: Asset]

  /** */
  trait Liability extends Credit
  val Liability: DtEnum[_ <: Liability]

  /** */
  trait Income extends Debit
  val Income: DtEnum[_ <: Income]

  /** */
  trait Equity extends Liability
  val Equity: DtEnum[_ <: Equity]

  /** instantiate double entry key module with appropriate monetary amount type */
  /** Mapping accounting keys to [[money.Money]]. */
  final type AccountMap[A <: AccountingKey, C] = Map[A, Mny[C]]

  /** */
  object AccountMap {

    /** */
    def empty[K <: AccountingKey, C: Currency]: AccountMap[K, C] = Map.empty

    /** */
    object implicits {

      /** */
      implicit final class MoarMapOps[K <: AccountingKey, V](m: Map[K, V]) {

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
        def collectKeys[L <: K](subKey: K => Option[L]): Map[L, V] =
          m collect (Function unlift { case (k, v) => subKey(k) map (l => (l, v)) })

        /** */
        def widenKeys[J >: K <: AccountingKey]: Map[J, V] = m.toMap // shrugs

        /** */
        def denominated[C: Currency](implicit V: Financial[V]): AccountMap[K, C] =
          m map {
            case (k, n) =>
              (k, Currency[C] fiat (Financial[V] to [MonetaryAmount] n))
          }
      }
    }
  }

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
  sealed trait NettableLike extends EnumEntry with Serializable {

    /** */
    type AssetType <: Asset

    /** */
    def gross: AssetType

    /** */
    def less: AssetType

    /** */
    final def net[C: Currency](assets: Assets[C]): Mny[C] =
      assets(gross) - assets(less)
  }

  /** */
  abstract class Nettable[D <: Asset](val gross: D, val less: D) extends NettableLike {
    final type AssetType = D
  }

  /**
    * We call the assingment of fractional amounts to certain accounting keys a ''treatment'',
    * following terminology common in the accounting field.
    */
  type Treatment[AK <: AccountingKey] = UnitPartition[AK, MonetaryAmount]

  /** */
  final val Treatment = UnitPartition

  /**
    * General form of a "double (bookkeeping) entry" key.
    */
  sealed trait DoubleEntryKeyLike extends EnumEntry with Product with Serializable {

    /** */
    type EntryType <: AccountingKey

    /** */
    type ContraType <: AccountingKey

    /** */
    def entries: Treatment[EntryType]

    /** */
    def contras: Treatment[ContraType]
  }

  /** */
  sealed abstract class DoubleEntryKey[ENTRY <: AccountingKey, CONTRA <: AccountingKey](
      es: Treatment[ENTRY],
      cs: Treatment[CONTRA]
  ) extends DoubleEntryKeyLike {

    /** */
    final type EntryType = ENTRY

    /** */
    final type ContraType = CONTRA

    /** */
    final def entries: Treatment[EntryType] = es

    /** */
    final def contras: Treatment[ContraType] = cs
  }

  /** placeholder */
  object DoubleEntryKey

  /** not sealed - extension point */
  abstract class DebitCreditKey[D <: Debit, C <: Credit](
      debits: Treatment[D],
      credits: Treatment[C]
  ) extends DoubleEntryKey(debits, credits) {
    final def debits: Treatment[D]  = entries
    final def credits: Treatment[C] = contras
  }

  /** placeholder */
  object DebitCreditKey

  /**
    * Keys that preserve the balance of a [[BalanceSheet]].
    *
    * `SwapKey`'s type parameter restricts the swap to occur
    * within the same "column" of the `Balance`.
    */
  abstract class SwapKey[T <: AccountingKey] private[model] (
      val from: Treatment[T],
      val to: Treatment[T]
  ) extends DoubleEntryKey(from, to)

  /** */
  object SwapKey {

    /** */
    def accountMap[A <: AccountingKey, C: Currency](
        ks: SwapKey[A],
        amount: Mny[C]
    ): AccountMap[A, C] = {
      def from = ks.from.toSortedMap mapValues (-amount * _)
      def to   = ks.to.toSortedMap mapValues (amount * _)
      from |+| to
    }

    def unapply[EE <: AccountingKey](sk: SwapKey[EE]): Option[(Treatment[EE], Treatment[EE])] =
      (sk.from, sk.to).some

  }

  /** */
  abstract class SimpleDebitCreditKey(
      final val debit: Debit,
      final val credit: Credit
  ) extends DebitCreditKey(Treatment single debit, Treatment single credit)

  /** */
  val SimpleDebitCreditKey: DtEnum[_ <: SimpleDebitCreditKey]
}
