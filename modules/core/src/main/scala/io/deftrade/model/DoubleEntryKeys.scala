package io.deftrade
package model

import money._, keyval.DtEnum

import cats.implicits._

import enumeratum._

/**
  * Modular cake layer which abstracts over [[money.Financial]] types.
  *
  * FIXME: breaking this into potentially polymorphic pieces is proving ponderous.
  */
abstract class DoubleEntryKeys[MA: Financial, Q: Financial] extends Pricing[MA, Q] with IRS1065 {

  /**
    * We call the assingment of fractional amounts to certain accounting keys a ''treatment'',
    * following terminology common in the accounting field.
    */
  type Treatment[AK <: AccountingKey] = UnitPartition[AK, MA]

  /** */
  final val Treatment = UnitPartition

  /**
    * General form of a "double (bookkeeping) entry" key.
    */
  sealed trait DoubleEntryKeyLike extends EnumEntry with Serializable {

    /** */
    type EntryType <: AccountingKey

    /** */
    type ContraType <: AccountingKey

    /** */
    def entries: Treatment[EntryType]

    /** */
    def contras: Treatment[ContraType]
  }

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

  /** */
  object DoubleEntryKey

  sealed abstract class DebitCreditKey[D <: Debit, C <: Credit](
      debits: Treatment[D],
      credits: Treatment[C]
  ) extends DoubleEntryKey(debits, credits) {
    final def debits: Treatment[D]  = entries
    final def credits: Treatment[C] = contras
  }
// }
// ///////////// would be separate sources, but for `sealed` /////////////
//
// abstract class DeksIRS1065Generic[MA: Financial] extends DoubleEntryKeys[MA] {

///// would be separate classes but dependent type issues tho... :(

  // /** */
  // sealed abstract class SingleDebitKey private (
  //     val debit: Debit,
  //     val credits: Treatment[Credit]
  // ) extends DebitCreditKey(Treatment single debit, credits)
  //
  // /** */
  // sealed abstract class SingleCreditKey private (
  //     val debits: Treatment[Debit],
  //     val credit: Credit
  // ) extends DebitCreditKey(debits, Treatment single credit)

  /** */
  sealed abstract class SimpleDebitCreditKey(
      final val debit: Debit,
      final val credit: Credit
  ) extends DebitCreditKey(Treatment single debit, Treatment single credit)

  /** Keys that grow or shrink the balance. */
  object SimpleDebitCreditKey extends DtEnum[SimpleDebitCreditKey] {

    /** */
    case object PayBill extends SimpleDebitCreditKey(Asset.Cash, Liability.AccountsPayable)

    /** */
    lazy val values = findValues
  }

  /**
    * Keys that preserve the balance of a [[BalanceSheet]].
    *
    * `SwapKey`'s type parameter restricts the swap to occur
    * within the same "column" of the `Balance`.
    */
  sealed abstract class SwapKey[T <: AccountingKey] private[model] (
      val from: Treatment[T],
      val to: Treatment[T]
  ) extends DoubleEntryKey(from, to)

  /** */
  object SwapKey {

    /** */
    def unapply[EE <: AccountingKey](sk: SwapKey[EE]): Option[(Treatment[EE], Treatment[EE])] =
      (sk.from, sk.to).some
  }

  /** */
  object AssetSwapKey extends DtEnum[SwapKey[Asset]] {

    import Asset._

    /** */
    case object ShipProduct extends SingleAssetSwapKey(Inventories, AccountsReceivable)

    /** */
    case object PurchaseInstrument extends SingleAssetSwapKey(OtherInvestments, Cash)

    /** */
    // lazy val values: IndexedSeq[SwapKey[Asset]] = findValues ++ SingleAssetSwapKey.values
    lazy val values = findValues
  }

  /** */
  sealed abstract class SingleAssetSwapKey private[model] (from: Asset, to: Asset)
      extends SwapKey[Asset](Treatment single from, Treatment single to)

  /** */
  object SingleAssetSwapKey {

    /** */
    def unapply[AK <: AccountingKey](sk: SwapKey[AK]): Option[(AK, AK)] =
      (sk.from, sk.to) match {
        case (UnitPartition.Single(f), UnitPartition.Single(t)) => (f, t).some
      }
  }

  /** */
  object LiabilitySwapKey extends DtEnum[SwapKey[Liability]] {

    import Liability._

    /** */
    case object LongTermToCurrentLiability
        extends SwapKey[Liability](
          Treatment single OtherLiabilities,
          Treatment single OtherCurrentLiabilities
        )

    /** */
    lazy val values = findValues
  }

  ///////////////// AccountMap.scala ///////////////////////

  /** instantiate double entry key module with appropriate monetary amount type */
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

}
