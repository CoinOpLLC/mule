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
package capital

import time.{ ZonedDateTime }
import time.market.Frequency
import money.{ Currency, CurrencyLike, Financial }
import keyval._
import refinements.{ Label }
import model.reference.{ IsIsin, IsUsin }

// import cats.implicits._
import cats.syntax.eq._
import cats.instances.string._

// import eu.timepit.refined.api.Refined

// import shapeless.syntax.singleton._

import enumeratum.EnumEntry

import io.circe.Json

import fs2.{ Pure, Stream }

/**
  * Models a tradeable thing.
  *
  * TODO:
  *   - use the XBRL definitions for these, a la OpenGamma
  *   - see implementations in `Refine` library
  *   - `symbol` implies a unified symbology. (These don't just happen.)
  */
final case class Instrument(
    symbol: Label,
    issuer: Party.Key,
    currency: CurrencyLike,
    meta: Json,
) {

  /** And by fiat we mean convention... */
  def isLegalTender: Boolean =
    symbol.value === currency.code.value
}

/**
  * `Instrument`s evolve over time.
  *
  * E.g. one might want to allow for, and track, a change in currency.
  * FIXME find some other way, this is nuts - when are new `ISIN`s issued?
  */
object Instrument extends WithRefinedKey[String, IsUsin, Instrument]

/**
  * WIP that follows Composing Contracts from Simon Peyton Jones et al.
  *
  * References:
  * [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ abandoned Haskell impl]]
  * TODO: finish
  */
sealed trait Contract

/** */
object Contract {

  case object Zero                                                             extends Contract
  sealed abstract case class One(k: CurrencyLike)                              extends Contract
  sealed abstract case class Give(c: Contract)                                 extends Contract
  sealed abstract case class When(o: Obs[Boolean], c: Contract)                extends Contract
  sealed abstract case class Until(o: Obs[Boolean], c: Contract)               extends Contract
  sealed abstract case class Upto(o: Obs[Boolean], c: Contract)                extends Contract
  sealed abstract case class Anytime(o: Obs[Boolean], c: Contract)             extends Contract
  sealed abstract case class Scale[N: Financial](o: Obs[N], c: Contract)       extends Contract
  sealed abstract case class And(c1: Contract, c2: Contract)                   extends Contract
  sealed abstract case class Or(c1: Contract, c2: Contract)                    extends Contract
  sealed abstract case class Cond(o: Obs[Boolean], c1: Contract, c2: Contract) extends Contract

  def zero: Contract                                              = Zero
  def one[C: Currency]: Contract                                  = new One(Currency[C]) {}
  def give(c: Contract): Contract                                 = new Give(c) {}
  def when(o: Obs[Boolean], c: Contract): Contract                = new When(o, c) {}
  def until(o: Obs[Boolean], c: Contract): Contract               = new Until(o, c) {}
  def upto(o: Obs[Boolean], c: Contract): Contract                = new Upto(o, c) {}
  def anytime(o: Obs[Boolean], c: Contract): Contract             = new Anytime(o, c) {}
  def scale[N: Financial](o: Obs[N], c: Contract): Contract       = new Scale(o, c) {}
  def and(c1: Contract, c2: Contract): Contract                   = new And(c1, c2) {}
  def or(c1: Contract, c2: Contract): Contract                    = new Or(c1, c2) {}
  def cond(o: Obs[Boolean], c1: Contract, c2: Contract): Contract = new Cond(o, c1, c2) {}

  /** */
  implicit class Ops(val c: Contract) extends AnyVal {

    final def give                           = Contract give c
    final def when(o: Obs[Boolean])          = Contract when (o, c)
    final def until(o: Obs[Boolean])         = Contract until (o, c)
    final def upto(o: Obs[Boolean])          = Contract upto (o, c)
    final def anytime(o: Obs[Boolean])       = Contract anytime (o, c)
    final def scale[N: Financial](o: Obs[N]) = Contract scale (o, c)
    final def and(c2: Contract)              = Contract and (c, c2)
    final def or(c2: Contract)               = Contract or (c, c2)
  }

  /** Close as we get to Haskell's `[]`. */
  type LazyList[A] = Stream[Pure, A]

  /** Random Variable */
  type RV[A] = LazyList[A]

  /** Value Process */
  private[capital] final case class PR[A] private (unPr: LazyList[RV[A]])

  /** primitives */
  object PR {

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def bigK[A](a: A): PR[A] = PR { (Stream emit (Stream emit a)).repeat }

    /** */
    def lift[A, B](f: A => B): PR[A] => PR[B] =
      pra => PR { pra.unPr map (_ map f) }

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def lift2[A, B, C](f: (A, B) => C): (PR[A], PR[B]) => PR[C] =
      (pra, prb) =>
        PR {
          pra.unPr.zipWith(prb.unPr) { (rva, rvb) =>
            rva.zipWith(rvb)(f)
          }
      }
  }

  /** Observable */
  case class Obs[A](f: ZonedDateTime => PR[A])

  /** */
  object Obs {

    /** `konst(x)` is an observable that has value x at any time. */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def konst[A](a: A): Obs[A] = Obs { _ =>
      PR bigK a
    }

    /** */
    def at(zdt: ZonedDateTime): Obs[Boolean] = ???

    // "The value of the observable date at date t is just t."
    //  date :: Obs Date
    //  date = Obs (\t -> PR $ timeSlices [t])

    // def time(zdt: ZonedDateTime): Obs[Period]      = ???
    // def wsjPrimeRate(date: LocalDate): Obs[Period] = ???
  }

  /**  */
  object Common {

    /**  */
    def zeroCouponBond[N: Financial, C: Currency](t: ZonedDateTime, x: N) =
      when(Obs at t, one scale (Obs konst x))
  }
}

/** */
object columns {

  /** */
  sealed trait Tracker { def members: Set[Instrument.Key] }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.)*/
  sealed trait Maturity { def matures: ZonedDateTime }

  /** */
  sealed trait Expiry { def expires: ZonedDateTime }

  /** All derivatives (e.g. Futures) are assumed to expire. */
  sealed trait Derivative extends Expiry { def underlier: WithKey#Key }

  /** Everyting with a strike is assumed to expire. */
  sealed trait Strike[N] { self: Derivative =>
    def strike: N
  }

  /** */
  object Strike {
    // def apply[N: Financial](strike: N): Strike[N]
    /** */
    object LogMoneynessStrike {
      // def apply[N: Financial](strike: N, forward: N): LogMoneynessStrike[N] =
      //   apply(ln(strike / forward)) TODO abstract ln over Financial[N] ?!
    }
  }
}

/** */
object layers {

  import columns._

  /** */
  trait PrimaryCapital {

    /** */
    case class CommonStock(
        tclass: Option[Label]
    )

    /** */
    object CommonStock extends WithRefinedKey[String, IsUsin, CommonStock]

    /** */
    case class PreferredStock(
        series: Label,
    )

    /** */
    object PreferredStock extends WithRefinedKey[String, IsUsin, PreferredStock]

    /** */
    case class Bond(
        override val matures: ZonedDateTime
    ) extends Maturity

    /**
      * `Bonds` (as opposed to loans) are always issued by corporations, never by natural persons.
      */
    object Bond extends WithRefinedKey[String, IsIsin, Bond]

    /** */
    case class TreasurySecurity(
        override val matures: ZonedDateTime
    ) extends Maturity
  }

  /**
    * And by "vanilla" we mean an exchange traded derivative (ETD).
    */
  trait VanillaDerivatives {

    sealed trait PutCall extends EnumEntry
    object PutCall extends DtEnum[PutCall] {
      case object Put  extends PutCall
      case object Call extends PutCall
      lazy val values = findValues
    }

    /** */
    case class Index(
        override val members: Set[Instrument.Key]
    ) extends Tracker

    /** */
    object Index extends WithRefinedKey[String, IsIsin, Index]

    /** Exchange Traded Derivative - Future (ETD) */
    case class EtdFuture(
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key
    ) extends Derivative

    object EtdFuture extends WithRefinedKey[String, IsIsin, EtdFuture]

    /**Exchange Traded Derivative - Option (ETD)  */
    case class EtdOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]

    /** I mean, right? */
    case class EtdFutureOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: EtdFuture.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]

    /** */
    case class EtdIndexOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Index.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]
  }

  /**
    * WIP - for otc derivative market participants (like hedge funds).
    */
  trait Exotics {

    /** A product only used for calibration. FIXME WTF */
    case class Calibration()

    /** Credit Default Swap */
    case class Cds()

    /** */
    case class CdsIndex()

    /** Constant Maturity Swap */
    case class Cms()

    /**
      * [[https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf Deliverable Swap Forward]]
      */
    case class Dsf()

    /** Forward Rate Agreement */
    case class Fra()

    /** A representation based on sensitivities. FIXME WTF */
    case class Sensitivities()

    /** */
    case class Swap()

    /** */
    case class Swaption()
  }

  /**
    * WIP - for fx derivative market participants (like banks).
    */
  trait Fx extends {

    /** FX Non-Deliverable Forward */
    case class FxNdf()

    /** */
    case class FxSingle()

    /** */
    case class FxSingleBarrierOption()

    /** */
    case class FxSwap()

    /** */
    case class FxVanillaOption()
  }

  /**
    * WIP - for otc derivative market participants (like banks).
    */
  trait Ibor extends {

    /** */
    case class IborCapFloor()

    /** */
    case class IborFuture()

    /** */
    case class IborFutureOption()
  }

  /**
    * Private lending instruments.
    *
    * TODO: do these next
    */
  trait Lending {

    /** */
    case class BulletPayment(
        instrument: Instrument.Key,
        matures: ZonedDateTime
    ) extends Maturity

    /** */
    case class CreditLine(
        instrument: Instrument.Key,
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1Q
    ) extends Maturity

    /** */
    case class AmortizingLoan(
        instrument: Instrument.Key,
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1M
    ) extends Maturity

    /** */
    case class ConvertibleNote()
  }
}
