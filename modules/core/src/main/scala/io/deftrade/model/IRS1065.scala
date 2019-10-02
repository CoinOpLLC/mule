package io.deftrade
package model

import keyval.DtEnum

import cats.implicits._

/**
  * IRS Form 1065 Schedule L ontology: partnerships and LLC's taxed as partnerships.
  */
trait AccountingIRS1065 extends Accounting { self: ModuleTypes =>

  /** */
  sealed trait Asset1065 extends Asset

  /** */
  object Asset extends DtEnum[Asset1065] {

    /** */
    lazy val values = findValues

    case object Cash                               extends Asset1065
    case object AccountsReceivable                 extends Asset1065
    case object LessBadDebtAllowance               extends Asset1065
    case object Inventories                        extends Asset1065
    case object USObligations                      extends Asset1065
    case object TaxExemptSec                       extends Asset1065
    case object OtherCurrent                       extends Asset1065
    case object LoansToPartners                    extends Asset1065
    case object MortgageAndRealEstateLoans         extends Asset1065
    case object OtherInvestments                   extends Asset1065
    case object BuildingsAndOtherDepreciableAssets extends Asset1065
    case object LessAccumulatedDepreciation        extends Asset1065
    case object DepletableAssets                   extends Asset1065
    case object LessAccumulatedDepletion           extends Asset1065
    case object Land                               extends Asset1065
    case object IntangibleAssets                   extends Asset1065
    case object LessAccumulatedAmortization        extends Asset1065
    case object OtherAssets                        extends Asset1065
  }

  sealed trait Liability1065 extends Liability

  /** */
  object Liability extends DtEnum[Liability1065] {

    case object AccountsPayable         extends Liability
    case object CurrentMortgateNotes    extends Liability
    case object OtherCurrentLiabilities extends Liability
    case object NonrecourseLoans        extends Liability
    case object LoansFromPartners       extends Liability
    case object MortgageNotes           extends Liability
    case object OtherLiabilities        extends Liability

    /** */
    lazy val values = findValues
  }

  sealed trait Equity1065 extends Equity

  /** */
  object Equity extends DtEnum[Equity1065] {

    case object PartnersCapital  extends Equity
    case object RetainedEarnings extends Equity

    /** */
    lazy val values = findValues
  }

  sealed trait Revenue1065 extends Revenue

  /** */
  object Revenue extends DtEnum[Revenue1065] {
    case object Finance        extends Revenue
    case object Investment     extends Revenue
    case object Receipts       extends Revenue
    case object OrdinaryIncome extends Revenue

    /** */
    lazy val values = findValues
  }

  sealed trait Expense1065 extends Expense

  /** */
  sealed trait OpEx extends Expense

  /** */
  sealed trait CapEx extends Expense

  /** */
  object Expense extends DtEnum[Expense1065] {
    case object Investment            extends CapEx
    case object Finance               extends CapEx // FIXME check this
    case object COGS                  extends OpEx
    case object RepairsAndMaintenance extends OpEx
    case object Salaries              extends OpEx
    case object Rent                  extends OpEx

    /** */
    lazy val values = findValues
  }

  sealed trait Income1065 extends Income

  /** FIXME: this needs work */
  object Income extends DtEnum[Income1065] {

    case object OperatingIncome  extends Income
    case object InvestmentIncome extends Income

    /** */
    lazy val values = findValues
  }

  /**
    * Depreciation, depletion, and amortization are the reasons some [[Asset]]s are Nettable.
    *
    * TODO:
    *   - Any other vocabularies to examine? Make this (differently) extensible?
    *   - Annoyance: why doesn't type inference work for Nettable?
    */
  object Nettable extends DtEnum[NettableLike] {

    import Asset._

    case object Depreciable
        extends Nettable[Asset](
          BuildingsAndOtherDepreciableAssets,
          LessAccumulatedDepreciation
        )

    case object Depletable
        extends Nettable[Asset](
          DepletableAssets,
          LessAccumulatedDepletion
        )

    case object Amortizable
        extends Nettable[Asset](
          IntangibleAssets,
          LessAccumulatedAmortization
        )

    val values = findValues
  }

  ///////////////////////////// Double Entry Key stuff /////////////////////////////

  /** */
  sealed abstract class SDCK1065(d: Debit, c: Credit) extends SimpleDebitCreditKey(d, c)

  /** Keys that grow or shrink the balance. */
  object SimpleDebitCreditKey extends DtEnum[SDCK1065] {

    /** */
    case object PayBill extends SDCK1065(Asset.Cash, Liability.AccountsPayable)

    /** */
    lazy val values = findValues
  }

  /** */
  sealed abstract class AssetSwapKey private[model] (
      f: Treatment[Asset],
      t: Treatment[Asset]
  ) extends SwapKey[Asset](f, t)

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
  object AssetSwapKey extends DtEnum[AssetSwapKey] {

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
  sealed abstract class LiabilitySwapKey private[model] (
      f: Treatment[Liability],
      t: Treatment[Liability]
  ) extends SwapKey[Liability](f, t)

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
    lazy val values = findValues
  }
}
