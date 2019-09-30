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
trait Accounting { self: ModuleTypeTraits =>

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
  sealed trait Expense extends Debit
  val Expense: DtEnum[Expense]

  /** */
  sealed trait Revenue extends Credit
  val Revenue: DtEnum[Revenue]

  /** */
  sealed trait Asset extends Debit
  val Asset: DtEnum[Asset]

  /** */
  sealed trait Liability extends Credit
  val Liability: DtEnum[Liability]

  /** */
  sealed trait Income extends Debit
  val Income: DtEnum[Income]

  /** */
  sealed trait Equity extends Liability
  val Equity: DtEnum[Equity]

  /** instantiate double entry key module with appropriate monetary amount type */
  /** Mapping accounting keys to [[money.Money]]. */
  final type AccountMap[A <: AccountingKey, C] = Map[A, Mny[C]]

  /** */
  object AccountMap {

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
  abstract class Nettable[D <: Asset](
      val gross: D,
      val less: D
  ) extends NettableLike { final type AssetType = D }

}

///////////// would be separate sources, but for `sealed` /////////////

/**
  * IRS Form 1065 Schedule L ontology: partnerships and LLC's taxed as partnerships.
  */
trait IRS1065 extends Accounting { self: ModuleTypeTraits =>

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
  object Equity extends DtEnum[Equity] {

    case object PartnersCapital  extends Equity
    case object RetainedEarnings extends Equity

    /** */
    lazy val values = findValues
  }

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

  /** FIXME: this needs work */
  object Income extends DtEnum[Income] {

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

}
