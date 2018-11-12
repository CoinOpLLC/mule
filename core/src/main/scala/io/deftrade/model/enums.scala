package io.deftrade
package model.enums

import cats.Eq

import enumeratum._
// import enumeratum.values._

sealed trait Role extends EnumEntry
object Role extends Enum[Role] {

  /**
    * The `Entity` which is the economic actor responsible for establishing the `Account`.
    *
    * Semantics for `Principle` are conditioned on the status of account, for examples:
    * - beneficial owner for an asset
    * - responsible party for a liability
    * - shareholder for equity
    * - business unit chief for revenue and expenses
    */
  case object Principle extends Role

  /**
    * The primary delegate selected by a `Principle`.
    * Also, simply, the `Entity`(s) whose names are listed on the `Account`,
    * and the primary point of contact for the `Account`.
    *
    * `Agents` have authortity to initiate `Transactions` which establish or remove `Position`s
    * from the `Ledger`.
    *
    * By convention a `Princple` is their own `Agent` unless otherwise specified.
    */
  case object Agent extends Role

  /**
    * The primary delegate selected by the `Agent`.
    * `Entity`(s) with responsibility for, and authority over,
    * the disposition of assets in the `Account`.
    *
    * In particular, `Manager`s may initiate `Transaction`s which will settle to the `Ledger`,
    * so long as the `Position`s are already entered in the `Ledger` - i.e. the `Instrument` is
    * known to be tradeable on the ledger.
    *
    * (All publicly listed and traded assets are treated as tradeable on the `Ledger`
    * by convention.)
    *
    * An `Agent` is their own `Manager` unless otherwise specified.
    */
  case object Manager extends Role

  /**
    * `Regulator`s are first class entities, each with a package of rights and responsibilities
    * which is situation and juristiction specific.
    *
    * Practically, what this means is that `Regulator`s will have a (possibly limited) view
    * into the state of the `Ledger`,
    * and (possibly) the ability to block the settlement of `Transaction`s to the `Ledger`
    * or even intitiate `Transaction`s.
    *
    * Actions of the `Regulator` may include the publishing of specific summaries of its views
    * into the `Ledger` to establish common knowledge for participants in `Ledger` `Transaction`s.
    *
    * N.B.: the `Regulator` need not be a governmental entity; in particular this role might
    * be suited to a risk manager function.
    */
  case object Regulator extends Role

  /** The `findValues` macro collects all `value`s in the order written. */
  lazy val values: IndexedSeq[Role] = findValues

  implicit lazy val eq = Eq.fromUniversalEquals[Role]
}

/** IRS Form 1065 Schedule L ontology */
/**
  Assets + eXpenses = eQuity + Liabilities + Income
  A + X = Q + L + I.
  */
sealed trait AccountType extends EnumEntry
object AccountType {
  implicit lazy val eq = Eq.fromUniversalEquals[AccountType]
}

sealed trait DebitAccount extends AccountType
object DebitAccount

sealed trait CreditAccount extends AccountType
object CreditAccount

sealed trait Asset extends DebitAccount
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

sealed trait LOQ extends CreditAccount

sealed trait Liability extends LOQ
object Liability extends Enum[Liability] {
  lazy val values = findValues
  case object AccountsPayable        extends Liability
  case object CurrentMortgateNotes   extends Liability
  case object OtherCurrentLabilities extends Liability
  case object NonrecourseLoans       extends Liability
  case object LoansFromPartners      extends Liability
  case object MortgageNotes          extends Liability
  case object OtherLiabilities       extends Liability
}

sealed trait Equity extends LOQ
object Equity extends Enum[Equity] {
  case object PartnersCapital extends Equity
  lazy val values = findValues
}

object LOQ extends Enum[LOQ] {
  lazy val values = Liability.values ++ Equity.values
}

sealed trait Revenue extends CreditAccount
object Revenue       extends Enum[Revenue] { lazy val values = findValues }

sealed trait XOP extends DebitAccount

sealed trait Expense extends XOP
object Expense       extends Enum[Expense] { lazy val values = findValues }

sealed trait Profit extends XOP
object Profit       extends Enum[Profit] { lazy val values = findValues }

object XOP extends Enum[XOP] {
  lazy val values = Expense.values ++ Profit.values
}

case class AccountTypeKey(debit: AccountType, credit: AccountType)
object AccountTypeKey {
  // def mk(debit: AccountType, credit: AccountType): AccountKeyType = (debit, credit) {}
  lazy val PurchaseInstrument = AccountTypeKey(Asset.OtherInvestments, Asset.Cash)
  lazy val PayBills           = AccountTypeKey(Liability.AccountsPayable, Asset.Cash)
  // etc.
}
