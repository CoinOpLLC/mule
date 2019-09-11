package io.deftrade
package model
package accounting

import money.Financial, keyval.DtEnum

import cats.implicits._

import enumeratum._

/**
  * Modular cake layer which abstracts over [[money.Financial]] types.
  * FIXME: total WIP.
  */
abstract class DoubleEntryKeys[MA: Financial] {

  /**
    * We call the assingment of fractional amounts to certain accounting keys a ''treatment'',
    * following usage common in the accounting field.
    */
  type Treatment[AK <: AccountingKey] = UnitPartition[AK, MA]

  /** */
  final val Treatment = UnitPartition

  /**
    * General form of a "double (bookkeeping) entry" key.
    */
  sealed abstract class DoubleEntryKey[X <: AccountingKey, Y <: AccountingKey] private[accounting] (
      entries: Treatment[X],
      contras: Treatment[Y]
  ) extends EnumEntry
      with Serializable {
    final type EntryType  = X
    final type ContraType = Y
  }

  /** namespace placeholder */
  object DoubleEntryKey

  /** */
  sealed abstract class SingleDebitKey private (
      val debit: Debit,
      val credits: Treatment[Credit]
  ) extends DoubleEntryKey(entries = Treatment single debit, contras = credits)

  /** */
  sealed abstract class SingleCreditKey private (
      val debits: Treatment[Debit],
      val credit: Credit
// ) extends DoubleEntryKey(entries = debits, contras = Treatment single credit)
  ) extends DoubleEntryKey(entries = Treatment single credit, contras = debits)

  /** Keys that grow or shrink the balance. */
  object SingleDebitKey extends DtEnum[SingleDebitKey] {

    /** */
    case object PayBills extends SingleDebitKey(Asset.Cash, Treatment single Liability.AccountsPayable)

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
      extends SwapKey[Asset](
        from = Treatment single from,
        to = Treatment single to
      )

  /** */
  object SingleAssetSwapKey extends DtEnum[SingleAssetSwapKey] {

    import Asset._

    /** */
    case object ShipProduct extends SingleAssetSwapKey(Inventories, AccountsReceivable)

    /** */
    case object PurchaseInstrument extends SingleAssetSwapKey(OtherInvestments, Cash)

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
