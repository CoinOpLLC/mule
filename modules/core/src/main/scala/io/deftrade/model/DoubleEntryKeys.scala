package io.deftrade
package model
package accounting

import money._, keyval.DtEnum

import cats.implicits._

import enumeratum._

/**
  * Modular cake layer which abstracts over [[money.Financial]] types.
  * FIXME: total WIP.
  */
abstract class DoubleEntryKeys[MA: Financial] {

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

  sealed abstract class DoubleEntryKey[E <: AccountingKey, C <: AccountingKey](
      es: Treatment[E],
      cs: Treatment[C]
  ) extends DoubleEntryKeyLike {

    /** */
    final type EntryType = E

    /** */
    final type ContraType = C

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
  sealed abstract class SwapKey[T <: AccountingKey] private[accounting] (
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
  sealed abstract class AssetSwapKey private[accounting] (
      from: Treatment[Asset],
      to: Treatment[Asset]
  ) extends SwapKey(from, to)

  /** */
  object AssetSwapKey extends DtEnum[AssetSwapKey] {

    /** */
    lazy val values = findValues
  }

  /** */
  sealed abstract class SingleAssetSwapKey private (from: Asset, to: Asset)
      extends SwapKey[Asset](Treatment single from, Treatment single to)

  /** */
  object SingleAssetSwapKey extends DtEnum[SingleAssetSwapKey] {

    import Asset._

    /** */
    case object ShipProduct extends SingleAssetSwapKey(Inventories, AccountsReceivable)

    /** */
    case object PurchaseInstrument extends SingleAssetSwapKey(OtherInvestments, Cash)

    /** */
    def unapply[AK <: AccountingKey](sk: SwapKey[AK]): Option[(AK, AK)] =
      (sk.from, sk.to) match {
        case (UnitPartition.Single(f), UnitPartition.Single(t)) => (f, t).some
      }

    /** */
    lazy val values = findValues
  }

  /** */
  sealed abstract class LiabilitySwapKey private[accounting] (
      from: Treatment[Liability],
      to: Treatment[Liability]
  ) extends SwapKey[Liability](from, to)

  /** */
  object LiabilitySwapKey extends DtEnum[LiabilitySwapKey] {

    import Liability._

    /** */
    case object LongTermToCurrentLiability
        extends LiabilitySwapKey(
          Treatment single OtherLiabilities,
          Treatment single OtherCurrentLiabilities
        )

    /** */
    def unapply[AK <: AccountingKey](sk: SwapKey[AK]): Option[(Treatment[AK], Treatment[AK])] =
      SwapKey.unapply[AK](sk)

    /** */
    lazy val values = findValues
  }
}
