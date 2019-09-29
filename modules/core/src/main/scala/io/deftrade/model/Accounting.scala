package io.deftrade
package model

import keyval.DtEnum

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
trait Accounting {

  /** */
  sealed trait AccountingKey extends EnumEntry with Serializable

  /** */
  object AccountingKey {

    /** this is just a hack to use `SortedSet`s  */
    implicit def orderKeys[AT <: AccountingKey]: cats.Order[AT] = cats.Order by (_.entryName)
  }

  /** */
  sealed trait Debit extends AccountingKey

  /** */
  object Debit extends DtEnum[Debit] {

    /** */
    lazy val values = Asset.values ++ Expense.values
  }

  /** */
  sealed trait Credit extends AccountingKey

  /** */
  object Credit extends DtEnum[Credit] {

    /** */
    lazy val values = Liability.values ++ Equity.values ++ Revenue.values
  }

  type Expense <: Debit
  val Expense: DtEnum[Expense]

  type Revenue <: Credit
  val Revenue: DtEnum[Revenue]

  type Asset <: Debit
  val Asset: DtEnum[Asset]

  type Liability <: Credit
  val Liability: DtEnum[Liability]

  type Income <: Debit
  val Income: DtEnum[Income]

  type Equity <: Credit
  val Equity: DtEnum[Equity]

}

///////////// would be separate sources, but for `sealed` /////////////

/**
  * IRS Form 1065 Schedule L ontology: partnerships and LLC's taxed as partnerships.
  */
trait IRS1065 extends Accounting {

  /** */
  sealed trait Asset extends Debit

  /** */
  object Asset extends DtEnum[Asset] {

    /** */
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

  /** */
  sealed trait Liability extends Credit

  /** */
  object Liability extends DtEnum[Liability] {

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

  /** */
  sealed trait Equity extends Liability

  /** */
  object Equity extends DtEnum[Equity] {

    case object PartnersCapital  extends Equity
    case object RetainedEarnings extends Equity

    /** */
    lazy val values = findValues
  }

  /** */
  sealed trait Revenue extends Credit

  /** */
  object Revenue extends DtEnum[Revenue] {
    case object Finance        extends Revenue
    case object Investment     extends Revenue
    case object Receipts       extends Revenue
    case object OrdinaryIncome extends Revenue

    /** */
    lazy val values = findValues
  }

  /** */
  sealed trait Expense extends Debit

  /** */
  sealed trait OpEx extends Expense

  /** */
  sealed trait CapEx extends Expense

  /** */
  object Expense extends DtEnum[Expense] {
    case object Investment            extends CapEx
    case object Finance               extends CapEx // FIXME check this
    case object COGS                  extends OpEx
    case object RepairsAndMaintenance extends OpEx
    case object Salaries              extends OpEx
    case object Rent                  extends OpEx

    /** */
    lazy val values = findValues
  }

  /** */
  sealed trait Income extends Debit

  /** FIXME: this needs work */
  object Income extends DtEnum[Income] {

    case object OperatingIncome  extends Income
    case object InvestmentIncome extends Income

    /** */
    lazy val values = findValues
  }
}
