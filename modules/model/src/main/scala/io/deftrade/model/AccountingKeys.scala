package io.deftrade
package model.enums

import cats.Eq

import enumeratum._

/** IRS Form 1065 Schedule L ontology */
/**
  Assets + eXpenses = eQuity + Liabilities + Income
  A + X = Q + L + I.
  */
sealed trait AccountType extends EnumEntry with Product with Serializable
object AccountType {
  implicit lazy val eq = Eq.fromUniversalEquals[AccountType]
}

sealed trait Debit extends AccountType
object Debit {
  lazy val values = Asset.values ++ XOP.values
}

sealed trait Credit extends AccountType
object Credit {
  lazy val values = LOQ.values ++ Revenue.values
}

sealed trait Asset extends Debit
object Asset extends Enum[Asset] {
  lazy val values = findValues
  case object Cash                               extends Asset
  case object AccountsReceivable                 extends Asset
  case object LessBadDebtAllowance               extends Asset
  case object Inventories                        extends Asset
  case object USObligations                      extends Asset
  case object TaxExemptSec                       extends Asset
  case object OtherCurrent                       extends Asset
  case object LoansToPartners                    extends Asset
  case object MortgageAndRealEstateLoans         extends Asset
  case object OtherInvestments                   extends Asset
  case object BuildingsAndOtherDepreciableAssets extends Asset
  case object LessAccumulatedDepreciation        extends Asset
  case object DepletableAssets                   extends Asset
  case object LessAccumulatedDepletion           extends Asset
  case object Land                               extends Asset
  case object IntangibleAssets                   extends Asset
  case object LessAccumulatedAmortization        extends Asset
  case object OtherAssets                        extends Asset
}

sealed trait LOQ extends Credit
object LOQ extends Enum[LOQ] {
  lazy val values = Liability.values ++ Equity.values
}

sealed trait Liability extends LOQ
object Liability extends Enum[Liability] {
  lazy val values = findValues
  case object AccountsPayable         extends Liability
  case object CurrentMortgateNotes    extends Liability
  case object OtherCurrentLiabilities extends Liability
  case object NonrecourseLoans        extends Liability
  case object LoansFromPartners       extends Liability
  case object MortgageNotes           extends Liability
  case object OtherLiabilities        extends Liability
}

sealed trait Equity extends LOQ
object Equity extends Enum[Equity] {
  case object PartnersCapital extends Equity
  lazy val values = findValues
}

sealed trait Revenue extends Credit
object Revenue extends Enum[Revenue] {
  case object Finance        extends Revenue
  case object Investment     extends Revenue
  case object Receipts       extends Revenue
  case object OrdinaryIncome extends Revenue

  lazy val values = findValues
}

sealed trait XOP extends Debit
object XOP extends Enum[XOP] {
  lazy val values = Expense.values ++ Profit.values
}

sealed trait Expense extends XOP
sealed trait OpEx    extends Expense
object Expense extends Enum[Expense] {
  case object Investment            extends Expense
  case object Finance               extends Expense
  case object COGS                  extends OpEx
  case object RepairsAndMaintenance extends OpEx
  case object Salaries              extends OpEx
  case object Rent                  extends OpEx
  lazy val values = findValues
}

sealed trait Profit extends XOP
object Profit extends Enum[Profit] {
  case object Profit extends Profit // SAY IT AGAIN
  lazy val values = findValues
}

sealed abstract class DoubleEntryKey protected (
    k1: AccountType,
    k2: AccountType
) extends EnumEntry
    with Product
    with Serializable
object DoubleEntryKey

sealed abstract class UpdateKey private (
    val debit: Debit,
    val credit: Credit
) extends DoubleEntryKey(debit, credit)
object UpdateKey extends Enum[UpdateKey] {
  case object PayBills extends UpdateKey(Asset.Cash, Liability.AccountsPayable)
  // etc.
  lazy val values = findValues
}

/**
  * `SwapKey`'s type parameter works to bind
  */
sealed abstract class SwapKey[T <: AccountType] private[enums] (
    val from: T,
    val to: T
) extends DoubleEntryKey(from, to)
object SwapKey

private[enums] sealed abstract class AssetSwapKey(from: Asset, to: Asset) extends SwapKey(from, to)
object AssetSwapKey extends Enum[AssetSwapKey] { // FIXME: profit?!

  import Asset._

  case object ShipProduct        extends AssetSwapKey(Inventories, AccountsReceivable)
  case object PurchaseInstrument extends AssetSwapKey(OtherInvestments, Cash)

  def unapply(ask: AssetSwapKey): Option[(Asset, Asset)] = Some(ask.from -> ask.to)
  lazy val values                                        = findValues
}
sealed abstract class LOQSwapKey(from: LOQ, to: LOQ) extends SwapKey(from, to)
object LOQSwapKey extends Enum[LOQSwapKey] { // FIXME: profit?!

  import Liability._, Equity._

  case object LongTermToCurrentLiability extends LOQSwapKey(OtherLiabilities, OtherCurrentLiabilities)

  def unapply(lsk: LOQSwapKey): Option[(LOQ, LOQ)] = Some(lsk.from -> lsk.to)
  lazy val values                                  = findValues
}

sealed abstract class Nettable(val gross: Asset, val less: Asset) extends EnumEntry with Product with Serializable {
  import io.deftrade.model.{ Balance, MonetaryAmount }
  final def net[D >: Asset <: Debit, C <: Credit](b: Balance[D, C]): MonetaryAmount =
    b.ds(gross) - b.ds(less) // TODO: this is typesafe, but not fool-proof.
}
object Nettable extends Enum[Nettable] {

  import Asset._

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
