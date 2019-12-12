/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package model
package augments

import layers._
import keyval.DtEnum

import cats.implicits._

/**
  * IRS Form 1065 Schedule L ontology: partnerships and LLC's taxed as partnerships.
  */
trait IRS1065 { self: ModuleTypes with Accounting =>

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

  /** */
  sealed trait Debt1065 extends Debt

  /** */
  object Debt extends DtEnum[Debt1065] {

    case object AccountsPayable         extends Debt1065
    case object CurrentMortgateNotes    extends Debt1065
    case object OtherCurrentLiabilities extends Debt1065
    case object NonrecourseLoans        extends Debt1065
    case object LoansFromPartners       extends Debt1065
    case object MortgageNotes           extends Debt1065
    case object OtherLiabilities        extends Debt1065

    case object AccruedExpenses      extends Debt1065 // file under OtherCurrentLiabilities
    case object Reserves             extends Debt1065 // file under OtherCurrentLiabilities
    case object DistributionsPayable extends Debt1065 // file under OtherCurrentLiabilities

    /** FIXME: need to fit Reserves into the mix */
    lazy val values = findValues // ++ Reserve.values
  }

  /** */
  sealed trait Equity1065 extends Equity

  /** */
  object Equity extends DtEnum[Equity1065] {

    case object PartnersCapital  extends Equity1065
    case object RetainedEarnings extends Equity1065

    /** */
    lazy val values = findValues
  }

  /** */
  sealed trait Revenue1065 extends Revenue

  /** */
  object Revenue extends DtEnum[Revenue1065] {
    case object Sales          extends Revenue1065
    case object OrdinaryIncome extends Revenue1065
    case object Finance        extends Revenue1065
    case object Investment     extends Revenue1065

    /** */
    lazy val values = findValues
  }

  /** */
  sealed trait Expense1065 extends Expense

  /** */
  object Expense extends DtEnum[Expense1065] {
    case object Investment            extends Expense1065
    case object Finance               extends Expense1065
    case object COGS                  extends Expense1065
    case object RepairsAndMaintenance extends Expense1065
    case object Salaries              extends Expense1065
    case object Rent                  extends Expense1065
    case object OtherRental           extends Expense1065

    /** */
    lazy val values = findValues
  }

  /** */
  sealed trait Income1065 extends Income

  /** TODO: this needs work */
  object Income extends DtEnum[Income1065] {

    /** ordinary business income Schedule K item 1 */
    case object Operating extends Income1065

    /** Real estate rental. */
    case object NetRentalPer8825 extends Income1065

    /** Net [[Expense.OtherRental]] to calculate other net rental income. */
    case object OtherGrossRental extends Income1065

    case object Interest          extends Income1065
    case object OrdinaryDividend  extends Income1065
    case object QualifiedDividend extends Income1065

    case object Investment extends Income1065

    /** */
    lazy val values = findValues
  }

  /**
    * As a [[Credit]], a `Reserve` can represent:
    *   - an equity portion,
    *   - a "bad debt" (asset) contra account
    *   - an estimated future liability
    *
    */
  sealed trait Reserve1065 extends Reserve

  /**
    * TODO: Define further, or move from IRS1065 and create another augment.
    */
  object Reserve extends DtEnum[Reserve1065] {

    case object BadLoan    extends Reserve1065
    case object OptionPool extends Reserve1065

    /** */
    lazy val values = findValues
  }

  /**
    * Depreciation, depletion, and amortization are the reasons some [[Asset]]s are Nettable.
    *
    * FIXME: Why aren't the "LessXyz" extries "contra-assets" (i.e. credits, technically)
    *
    * TODO:
    *   - Any other vocabularies to examine? Make this (differently) extensible?
    *   - Annoyance: why doesn't type inference work for Nettable?
    */
  object Nettables extends DtEnum[Nettable] {

    import Asset._

    case object Defaultable
        extends Nettable.Aux[Asset](
          AccountsReceivable,
          LessBadDebtAllowance
        )

    case object Depreciable
        extends Nettable.Aux[Asset](
          BuildingsAndOtherDepreciableAssets,
          LessAccumulatedDepreciation
        )

    case object Depletable
        extends Nettable.Aux[Asset](
          DepletableAssets,
          LessAccumulatedDepletion
        )

    case object Amortizable
        extends Nettable.Aux[Asset](
          IntangibleAssets,
          LessAccumulatedAmortization
        )

    val values = findValues
  }

  ///////////////////////////// Double Entry Key stuff /////////////////////////////

  /** */
  sealed abstract class SingleDebitCreditKey(d: Debit, c: Credit)
      extends DebitCreditKey[Debit, Credit](
        Treatment single d,
        Treatment single c
      )

  /** Keys that grow or shrink the balance. */
  object SingleDebitCreditKey extends DtEnum[SingleDebitCreditKey] {

    /** */
    case object PayBill extends SingleDebitCreditKey(Asset.Cash, Debt.AccountsPayable)

    /** */
    case object BorrowShort extends SingleDebitCreditKey(Asset.Cash, Debt.OtherCurrentLiabilities)

    /** FIXME how to distinguish between borrow and payback? */
    case object BorrowLong extends SingleDebitCreditKey(Asset.Cash, Debt.OtherLiabilities)

    /** */
    case object SellStock extends SingleDebitCreditKey(Asset.Cash, Equity.PartnersCapital)

    /** apply to trial balance */
    case object SellProduct extends SingleDebitCreditKey(Asset.AccountsReceivable, Revenue.Sales)

    /** FIXME: need new class `DebitSwapKey` - this is a swap within a Trial Balance */
    case object CostProduct extends DebitSwapKey //(Asset.Inventories, Expense.COGS)

    /** */
    lazy val values = findValues
  }

  /** */
  sealed abstract class AssetSwapKey private[model] (
      f: Treatment[Asset],
      t: Treatment[Asset]
  ) extends SwapKey[Asset](f, t)

  /** */
  object AssetSwapKey extends DtEnum[AssetSwapKey] {

    /**  */
    lazy val values = findValues ++ SingleAssetSwapKey.values
  }

  /** */
  sealed abstract class SingleAssetSwapKey private[model] (from: Asset, to: Asset)
      extends AssetSwapKey(Treatment single from, Treatment single to)

  /** */
  object SingleAssetSwapKey extends DtEnum[SingleAssetSwapKey] {

    import Asset._

    lazy val values = findValues

    /** */
    def unapply[K <: AccountingKey](sk: SwapKey[K]): Option[(K, K)] =
      (sk.from, sk.to) match {
        case (UnitPartition.Single(f), UnitPartition.Single(t)) => (f, t).some
      }

    /** */
    case object MakeProduct extends SingleAssetSwapKey(Cash, Inventories)

    /** */
    case object ReceiveProductPayment extends SingleAssetSwapKey(AccountsReceivable, Cash)

    /** */
    case object BuyInstrument extends SingleAssetSwapKey(OtherInvestments, Cash)
  }

  /** */
  sealed abstract class LiabilitySwapKey private[model] (
      f: Treatment[Liability],
      t: Treatment[Liability]
  ) extends SwapKey[Liability](f, t)

  /** */
  object LiabilitySwapKey extends DtEnum[LiabilitySwapKey] {

    /** */
    lazy val values = findValues ++ SingleLiabilitySwapKey.values
  }

  /** */
  sealed abstract class SingleLiabilitySwapKey private[model] (from: Liability, to: Liability)
      extends LiabilitySwapKey(Treatment single from, Treatment single to)

  /** */
  object SingleLiabilitySwapKey extends DtEnum[SingleLiabilitySwapKey] {

    import Debt._

    /** */
    def unapply[K <: AccountingKey](sk: SwapKey[K]): Option[(K, K)] =
      (sk.from, sk.to) match {
        case (UnitPartition.Single(f), UnitPartition.Single(t)) => (f, t).some
      }

    /** */
    case object MakeCurrent
        extends SingleLiabilitySwapKey(
          OtherLiabilities,
          OtherCurrentLiabilities
        )

    lazy val values = findValues
  }
}
