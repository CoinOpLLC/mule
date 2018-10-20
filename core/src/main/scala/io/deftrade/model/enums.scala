package io.deftrade
package model.enums

import cats.{ Eq, Order }
import cats.instances.string._
import enumeratum._
// import enumeratum.values._

sealed trait UniversalInstrumentIdentifyer extends EnumEntry { def s: String }
object UniversalInstrumentIdentifyer extends Enum[UniversalInstrumentIdentifyer] {
  // TODO: each of these needs a regex refinement for the string param
  case class Cusip(s: String)        extends UniversalInstrumentIdentifyer
  case class Isin(s: String)         extends UniversalInstrumentIdentifyer
  case class Ric(s: String)          extends UniversalInstrumentIdentifyer
  case class Buid(s: String)         extends UniversalInstrumentIdentifyer
  case class IbContractId(s: String) extends UniversalInstrumentIdentifyer
  case class HouseId(s: String)      extends UniversalInstrumentIdentifyer

  lazy val values = findValues // FIXME: what the hell does this do?

  // implicit lazy val eq = Eq.fromUniversalEquals[UniversalInstrumentIdentifyer]
  implicit lazy val order: Order[UniversalInstrumentIdentifyer] = Order by (_.s)
}

sealed trait Derivative { self: Instrument =>
  def underlyer: Instrument
}
object Derivative

sealed trait Instrument extends EnumEntry { def usi: UniversalInstrumentIdentifyer }

object Instrument extends Enum[Instrument] {

  type USI = UniversalInstrumentIdentifyer

  lazy val values = findValues // FIXME: test this lol... case *classes* render ambiguous

  // A FixedCouponBond or CapitalIndexedBond.
  case class Bond(val usi: USI) extends Instrument
  // A BondFuture.
  case class BondFuture(val usi: USI) extends Instrument
  // A BondFutureOption.
  case class BondFutureOption(val usi: USI) extends Instrument
  // A BulletPayment.
  case class BulletPayment(val usi: USI) extends Instrument
  // A product only used for calibration.
  case class Calibration(val usi: USI) extends Instrument
  // CreditAccount Default Swap (CDS)
  case class Cds(val usi: USI) extends Instrument
  // CDS index
  case class CdsIndex(val usi: USI) extends Instrument
  // Constant Maturity Swap (CMS)
  case class Cms(val usi: USI) extends Instrument
  // A Deliverable Swap Forward
  // https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf
  case class Dsf(val usi: USI) extends Instrument
  // Exchange Traded Derivative - Future (ETD)
  case class EtdFuture(val usi: USI) extends Instrument // FIXME: this conflicts wtih mine...
  // Exchange Traded Derivative - Option (ETD)
  case class EtdOption(val usi: USI) extends Instrument
  // Forward Rate Agreement
  case class Fra(val usi: USI) extends Instrument
  // FX Non-Deliverable Forward
  case class FxNdf(val usi: USI) extends Instrument
  // A FxSingle.
  case class FxSingle(val usi: USI) extends Instrument
  // A FxSingleBarrierOption.
  case class FxSingleBarrierOption(val usi: USI) extends Instrument
  // A FxSwap.
  case class FxSwap(val usi: USI) extends Instrument
  // A FxVanillaOption.
  case class FxVanillaOption(val usi: USI, val underlyer: Instrument) extends Instrument with Derivative
  // A IborCapFloor.
  case class IborCapFloor(val usi: USI) extends Instrument
  // A IborFuture.
  case class IborFuture(val usi: USI) extends Instrument
  // A IborFutureOption.
  case class IborFutureOption(val usi: USI) extends Instrument
  // // A representation based on sensitivities.
  case class Sensitivities(val usi: USI) extends Instrument
  // A Swap.
  case class Swap(val usi: USI) extends Instrument
  // A Swaption.
  case class Swaption(val usi: USI) extends Instrument
  // A TermDeposit.
  case class TermDeposit(val usi: USI) extends Instrument

  case class AmortizingLoan(val usi: USI)  extends Instrument
  case class ConvertibleLoan(val usi: USI) extends Instrument

  case class CommonStock(val usi: USI)    extends Instrument
  case class PreferredStock(val usi: USI) extends Instrument

  case class StockIndexFutureOption(val usi: USI, val underlyer: Instrument) extends Instrument with Derivative
  case class StockIndexOption(val usi: USI, val underlyer: Instrument)       extends Instrument with Derivative
  case class StockIndexFuture(val usi: USI, val underlyer: Instrument)       extends Instrument with Derivative
  case class StockOption(val usi: USI, val underlyer: Instrument)            extends Instrument with Derivative

  case class FxForwardSpot(val usi: USI) extends Instrument // FIXME not sure here

  implicit lazy val order: Order[Instrument] = Order by (_.usi)
}

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
    * so long as the `Position`s are already entered in the `Ledger`.
    *
    * (All publicly listed and traded assets are treated as entered into the `Ledger`
    * by convention.)
    *
    * By convention an `Agent` is their own `Manager` unless otherwise specified.
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

sealed trait Liability extends CreditAccount
object Liability extends Enum[Liability] {
  lazy val values = findValues
  case object AccountsPayable        extends Liability
  case object CurrentMortgateNotes   extends Liability
  case object OtherCurrentLabilities extends Liability
  case object NonrecourseLoans       extends Liability
  case object LoansFromPartners      extends Liability
  case object MortgageNotes          extends Liability
  case object OtherLiabilities       extends Liability
  case object PartnersCapital        extends Liability
}

sealed trait Equity extends CreditAccount
object Equity extends Enum[Equity] {
  lazy val values = findValues
}

sealed trait Revenue extends CreditAccount
object Revenue extends Enum[Revenue] {
  lazy val values = findValues
}

sealed trait Expenses extends DebitAccount
object Expenses extends Enum[Expenses] {
  lazy val values = findValues
}
