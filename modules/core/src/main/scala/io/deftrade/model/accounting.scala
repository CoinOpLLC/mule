package io.deftrade
package model

/**
  * IRS Form 1065 Schedule L ontology.
  *
  * Note: "partnershiptaxaccounting" would be a more accurate name for this package. :|
  */
package accounting

import cats.implicits._
import cats.data.NonEmptySet

import enumeratum._

/** IRS Form 1065 Schedule L ontology: partnerships and `LLC`s taxed as partnerships.
  {{{
   Assets + eXpenses = eQuity + Liabilities + Income
   A + X = Q + L + I.
  }}}
  */
sealed trait AccountingKey extends EnumEntry with Serializable
object AccountingKey {

  /** this is just a hack to use `SortedSet`s etc */
  implicit def orderKeys[AT <: AccountingKey]: cats.Order[AT] = cats.Order by (_.entryName)

}

sealed trait Debit extends AccountingKey
object Debit {
  lazy val values = Asset.values ++ Expense.values ++ Income.values
}

sealed trait Credit extends AccountingKey
object Credit {
  lazy val values = Liability.values ++ Equity.values ++ Revenue.values
}

sealed trait Asset extends Debit
object Asset extends Enum[Asset] with CatsEnum[Asset] {
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

sealed trait Liability extends Credit
object Liability extends Enum[Liability] with CatsEnum[Liability] {
  lazy val values = findValues
  case object AccountsPayable         extends Liability
  case object CurrentMortgateNotes    extends Liability
  case object OtherCurrentLiabilities extends Liability
  case object NonrecourseLoans        extends Liability
  case object LoansFromPartners       extends Liability
  case object MortgageNotes           extends Liability
  case object OtherLiabilities        extends Liability
}

sealed trait Equity extends Liability
object Equity extends Enum[Equity] with CatsEnum[Equity] {
  case object PartnersCapital  extends Equity
  case object RetainedEarnings extends Equity
  lazy val values = findValues
}

sealed trait Revenue extends Credit
object Revenue extends Enum[Revenue] with CatsEnum[Revenue] {
  case object Finance        extends Revenue
  case object Investment     extends Revenue
  case object Receipts       extends Revenue
  case object OrdinaryIncome extends Revenue

  lazy val values = findValues
}

sealed trait Expense extends Debit
sealed trait OpEx    extends Expense
sealed trait CapEx   extends Expense
object Expense extends Enum[Expense] with CatsEnum[Expense] {
  case object Investment            extends CapEx
  case object Finance               extends CapEx // FIXME check this
  case object COGS                  extends OpEx
  case object RepairsAndMaintenance extends OpEx
  case object Salaries              extends OpEx
  case object Rent                  extends OpEx
  lazy val values = findValues
}

sealed trait Income extends Debit
object Income extends Enum[Income] with CatsEnum[Income] {
  case object Income extends Income // SAY IT AGAIN
  lazy val values = findValues
}

object DoubleEntryKey {
  type KeySet[AT <: AccountingKey] = NonEmptySet[AT]
  final val KeySet = NonEmptySet
}
import DoubleEntryKey.KeySet

/** Single amount principle: one leg is singular */
sealed abstract class DoubleEntryKey[X <: AccountingKey, Y <: AccountingKey] private[accounting] (
    entries: KeySet[X],
    contras: KeySet[Y]
) extends EnumEntry
    with Serializable

sealed abstract class DebitKey private (
    val debit: Debit,
    val credits: KeySet[Credit]
) extends DoubleEntryKey[Debit, Credit](entries = KeySet one debit, contras = credits)

sealed abstract class CreditKey private (
    val debits: KeySet[Debit],
    val credit: Credit
) extends DoubleEntryKey[Debit, Credit](entries = debits, contras = KeySet one credit)

/** Keys that grow or shrink the balance. */
object DebitKey extends Enum[DebitKey] {

  import cats.instances.string._

  /** bill payment */
  case object PayBills extends DebitKey(Asset.Cash, KeySet one Liability.AccountsPayable)

  // etc.
  lazy val values = findValues
}

/**
  * Keys that preserve the balance.
  *
  * `SwapKey`'s type parameter restricts the swap to occur
  * within the same "column" of the `Balance`.
  */
sealed abstract class SwapKey[T <: AccountingKey] private[accounting] (
    val from: KeySet[T],
    val to: KeySet[T]
) extends DoubleEntryKey(from, to)

/** */
object SwapKey

private[accounting] sealed abstract class AssetSwapKey(from: Asset, to: Asset)
    extends SwapKey[Asset](
      from = KeySet one from,
      to = KeySet one to
    )

object AssetSwapKey extends Enum[AssetSwapKey] {

  import Asset._

  case object ShipProduct        extends AssetSwapKey(Inventories, AccountsReceivable)
  case object PurchaseInstrument extends AssetSwapKey(OtherInvestments, Cash)

  def unapply(ask: AssetSwapKey): Option[(Asset, Asset)] = Some(ask.from.head -> ask.to.head)

  lazy val values = findValues
}

sealed abstract class LiabilitySwapKey(
    from: Liability,
    to: KeySet[Liability]
) extends SwapKey[Liability](KeySet one from, to)
object LiabilitySwapKey extends Enum[LiabilitySwapKey] {

  import Liability._

  case object LongTermToCurrentLiability
      extends LiabilitySwapKey(
        OtherLiabilities,
        NonEmptySet one OtherCurrentLiabilities
      )

  // def unapply(lsk: LOQSwapKey): Option[(LOQ, LOQ)] = Some(lsk.from -> lsk.to)
  lazy val values = findValues
}
