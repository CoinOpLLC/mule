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

import io.deftrade.money.{ Financial, Monetary, QuotedIn }, Monetary.{ Monetary, Money }
// import io.deftrade.money._, Monetary._
import io.deftrade.time._

import cats.{ Eq, Monad, Monoid }
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
import refined.numeric._
// import refined.auto._

import enumeratum._
// import enumeratum.values._

// import io.circe.Json

import scala.language.higherKinds

import opaqueid._

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  */
abstract class Api[MonetaryAmount: Financial, Quantity: Financial] {

  /** Domain specific tools for dealing with `Quantity`s */
  val Quantity = Financial[Quantity]

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  val MonetaryAmount = Financial[MonetaryAmount]

  sealed trait Derivative { self: Security =>
    def underlyer: Security = ???
    def delta: BigDecimal   = ???
  }
  object Derivative

  sealed trait UniversalSecurityIdentifyer extends EnumEntry { def s: String }
  object UniversalSecurityIdentifyer extends Enum[UniversalSecurityIdentifyer] {

    case class Cusip(s: String)        extends UniversalSecurityIdentifyer
    case class Isin(s: String)         extends UniversalSecurityIdentifyer
    case class Ric(s: String)          extends UniversalSecurityIdentifyer
    case class Buid(s: String)         extends UniversalSecurityIdentifyer
    case class IbContractId(s: String) extends UniversalSecurityIdentifyer
    case class HouseId(s: String)      extends UniversalSecurityIdentifyer

    lazy val values = findValues

    implicit lazy val eq = Eq.fromUniversalEquals[UniversalSecurityIdentifyer]
  }

  type USI = UniversalSecurityIdentifyer
  val USI = UniversalSecurityIdentifyer

  // TODO: use the XBRL definitions for these, a la OpenGamma
  sealed trait Security extends EnumEntry { def usi: USI }
  object Security extends Enum[Security] {

    lazy val values = findValues

    // A FixedCouponBond or CapitalIndexedBond.
    case class Bond(val usi: USI) extends Security
    // A BondFuture.
    case class BondFuture(val usi: USI) extends Security
    // A BondFutureOption.
    case class BondFutureOption(val usi: USI) extends Security
    // A BulletPayment.
    case class BulletPayment(val usi: USI) extends Security
    // A product only used for calibration.
    case class Calibration(val usi: USI) extends Security
    // Credit Default Swap (CDS)
    case class Cds(val usi: USI) extends Security
    // CDS index
    case class CdsIndex(val usi: USI) extends Security
    // Constant Maturity Swap (CMS)
    case class Cms(val usi: USI) extends Security
    // A Dsf.
    case class Dsf(val usi: USI) extends Security
    // Exchange Traded Derivative - Future (ETD)
    case class EtdFuture(val usi: USI) extends Security // FIXME: this conflicts wtih mine...
    // Exchange Traded Derivative - Option (ETD)
    case class EtdOption(val usi: USI) extends Security
    // Forward Rate Agreement
    case class Fra(val usi: USI) extends Security
    // FX Non-Deliverable Forward
    case class FxNdf(val usi: USI) extends Security
    // A FxSingle.
    case class FxSingle(val usi: USI) extends Security
    // A FxSingleBarrierOption.
    case class FxSingleBarrierOption(val usi: USI) extends Security
    // A FxSwap.
    case class FxSwap(val usi: USI) extends Security
    // A FxVanillaOption.
    case class FxVanillaOption(val usi: USI) extends Security with Derivative
    // A IborCapFloor.
    case class IborCapFloor(val usi: USI) extends Security
    // A IborFuture.
    case class IborFuture(val usi: USI) extends Security
    // A IborFutureOption.
    case class IborFutureOption(val usi: USI) extends Security
    // // A representation based on sensitivities.
    case class Sensitivities(val usi: USI) extends Security
    // A Swap.
    case class Swap(val usi: USI) extends Security
    // A Swaption.
    case class Swaption(val usi: USI) extends Security
    // A TermDeposit.
    case class TermDeposit(val usi: USI) extends Security

    case class AmortizingLoan(val usi: USI)  extends Security
    case class ConvertibleLoan(val usi: USI) extends Security

    case class CommonStock(val usi: USI)    extends Security
    case class PreferredStock(val usi: USI) extends Security

    case class StockIndexFutureOption(val usi: USI) extends Security with Derivative
    case class StockIndexOption(val usi: USI)       extends Security with Derivative
    case class StockIndexFuture(val usi: USI)       extends Security with Derivative
    case class StockOption(val usi: USI)            extends Security with Derivative

    case class FxForwardSpot(val usi: USI) extends Security // FIXME not sure here

    implicit lazy val eq = Eq.fromUniversalEquals[Security]
  }

  type SecurityId = OpaqueId[Long, Security]
  object SecurityId extends OpaqueIdC[SecurityId]

  type Position = (SecurityId, Quantity)
  object Position

  type Leg = Position // `Leg` := `Position` in motion
  val Leg = Position

  type Folio = Map[SecurityId, Quantity] // n.b algebraic relationship Position <=> Folio
  object Folio {
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  type Trade = Folio // `Trade` := `Folio` in motion
  val Trade = Folio // gives you `apply`: Trade(legs: Leg*): Trade

  type FolioId = OpaqueId[Long, Folio]
  object FolioId extends OpaqueIdC[FolioId]

  /**
    * (Runtime) invariant: `Trade`s must balance across all the `FolioId`s in the
    * `LedgerEntry`.
    */
  type LedgerEntry = Map[FolioId, Trade]
  object LedgerEntry

  type Transaction = LedgerEntry
  val Transaction = LedgerEntry

  /**
    * What is the `Ledger`, anyway? It's a `Log` structure, essentially – Foldable[LedgerEntry]
    */
  type Ledger = List[LedgerEntry] // if you must - but reserving the name is a good idea regardless
  object Ledger

  /** Nota Bene: looks like LedgerState and LedgerEntry are the same thing (and form a monoid) */
  type LedgerState = Map[FolioId, Folio]
  object LedgerState

  trait Person
  trait Corporation

  type Entity = Either[Corporation, Person]
  object Entity

  type EntityId = OpaqueId[Long, Entity]
  object EntityId extends OpaqueIdC[EntityId]

  sealed trait Role extends EnumEntry
  object Role extends Enum[Role] {

    /**
      * The `Entity` which is the economic actor responsible for establishing the `Account`.
      *
      * Semantics for `Principle` are conditioned on the status of account:
      * - responsible party (liability)
      * - beneficial owner (asset)
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
      * In particular, `Manager`s may intitiate `Transaction`s which will settle to the `Ledger`,
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
  }

  /**
    * The portion of the total accorded to the identified `Entity`.
    */
  type Share = (EntityId, Quantity)
  object Share {
    import Quantity._
    def validate(p: Share): Boolean = {
      val (_, q) = p
      zero <= q && q <= one
    }
    def apply(eid: EntityId, q: Quantity): Share = (eid, q) ensuring { p =>
      validate(p)
    }
    def single(eid: EntityId) = Share(eid, one)
  }

  /**
    * Note that Roster encodes the notion of a `CapitalKey` – per `Role`!
    * The meaning of this `Partition` will depend upon the role.
    * For the beneficial owner(s) of the assets in question the meaning is obvious.
    * For a `Manager`, the `Partition` could encode their voting rights – which may differ from
    * their ownership rights.
    */
  type Partition = Map[EntityId, Quantity]
  object Partition {
    def apply(shares: Share*): Either[String, Partition] = {
      val p = accumulate(shares.toList)
      p.values.toList.sum match {
        case sum if sum == Quantity.one => p.asRight
        case badsum                     => s"Partition doesn't add up: $badsum != 1.0".asLeft
      }
    }
    def single(eid: EntityId): Partition = Map.empty + Share.single(eid)
  }

  type Roster = Map[Role, Partition]
  object Roster {
    def apply(eid: EntityId): Roster =
      Map.empty + (Role.Principle -> Partition.single(eid))
  }

  type Account = Map[FolioId, Roster]
  object Account {
    def updatedProRata(a: Account, newProRata: Map[EntityId, Quantity]): Account = ???
  }

  type AccountId = Long Refined Interval.Closed[W.`100000100100`, W.`999999999999`]
  object AccountId

  type LedgerKey = Map[AccountId, Account]
  object LedgerKey {

    def recorded(lk: LedgerKey)(ls: LedgerState)(ao: AllocatedOrder): LedgerState = ao match {
      case (_, transaction) =>
        transaction.toList foldMap {
          case (folioId, trade) =>
            (ls get folioId).fold(
              ls + (folioId -> trade)
            ) { folio =>
              ls + (folioId -> (folio |+| trade))
            }
        }
    }
  }

  type LedgerId = OpaqueId[Long, LedgerKey]
  object LedgerId extends OpaqueIdC[LedgerId]

  type Allocation = Map[FolioId, Quantity] // must sum to 1... TODO typesafe normalization?
  object Allocation {

    import Quantity._

    def allocate(allocation: Allocation)(order: Order): AllocatedOrder = ???

    def pariPassu(fids: Set[FolioId]): Allocation = {
      val n = fids.size
      (fids.toList zip List.fill(n)(one / fromInt(n))).toMap
    }
    def proRata(totalShares: Long)(capTable: Map[FolioId, Long]): Allocation =
      capTable map {
        case (folioId, shares) =>
          val mySlice  = fromLong(shares)
          val wholePie = fromLong(totalShares)
          (folioId, mySlice / wholePie)
      }

    object Single {
      def apply(folioId: FolioId): Allocation = Map(folioId -> one)
      def unapply(a: Allocation) = a.toList match {
        case (folioId, One) :: Nil => folioId.some
        case _                     => none
      }
    }
  }

  type Order = (AccountId, EntityId, Trade)
  object Order {
    import io.deftrade.money.Monetary.USD
    def legacy(bd: BigDecimal, q: Long): PricedTrade[USD] =
      (
        Map(LongId.reserved -> (Financial[Quantity] fromBigDecimal BigDecimal(q))),
        USD(Financial[MonetaryAmount] fromBigDecimal bd)
      )
  }

  type OrderId = OpaqueId[Long, Order]
  object OrderId extends OpaqueIdC[OrderId]

  type AllocatedOrder = (AccountId, Transaction)
  object AllocatedOrder

  // TODO: type constructors all the way?
  // how to capture the fact that `[CCY: Monetary]`
  type Execution[CCY] = (OrderId, LocalDateTime, PricedTrade[CCY])
  object Execution

  type ExecutionId[CCY] = OpaqueId[Long, Execution[CCY]]
  class ExecutionIdC[CCY] extends OpaqueIdC[ExecutionId[CCY]]
  // FIXME this requires custom fuzting

  type PricedTrade[CCY] = (Trade, Money[MonetaryAmount, CCY])
  object PricedTrade

  sealed trait Exchange // a multiparty nexus
  object Exchange

  sealed trait Counter // a single counterparty or market maker
  object Counter

  type Market = Either[Counter, Exchange]
  object Market {

    /** Price all the things. TODO: this feels like a repository */
    def quote[C: Monetary](m: Market)(id: SecurityId): Money[MonetaryAmount, C] =
      Monetary[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C: Monetary](m: Market)(id: SecurityId): SecurityId QuotedIn C = ???

    // idea: use phantom types to capture sequencing constraint?
    def trade[F[_]: Monad, CCY: Monetary](m: Market): PricedTrade[CCY] => F[Seq[Execution[CCY]]] =
      ???
  }

  type MarketId = OpaqueId[Long, Market]
  object MarketId extends OpaqueIdC[MarketId]

  def quoteLeg[CCY: Monetary](market: Market)(leg: Leg): Money[MonetaryAmount, CCY] =
    leg match {
      case (security, quantity) => Market.quote[CCY](market)(security) * quantity
    }

  def quote[CCY: Monetary](market: Market)(trade: Trade): Money[MonetaryAmount, CCY] =
    trade.toList foldMap quoteLeg[CCY](market)

  def index[K, V](kvs: Traversable[(K, V)]): Map[K, Traversable[V]] =
    kvs groupBy (_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  /** N.B. that `accumulate` requires a List parameter; `index` does not. */
  def accumulate[K, V: Monoid](kvs: List[(K, V)]): Map[K, V] =
    for {
      (k, kvs) <- kvs groupBy (_._1)
    } yield (k, kvs foldMap (_._2))

}

/**
 Leave these here. Opaque means opaque.
  */
object opaqueid {

  private[model] sealed trait IdTypeTraits extends Any {
    type Value
    type Phantom
  }

  private[model] sealed trait Id[V] extends Any with IdTypeTraits {
    final type Value = V
    def id: V
  }

  final case class OpaqueId[V, P] private[model] (val id: V) extends AnyVal with Id[V] {
    final type Phantom = P
  }

  object OpaqueId {
    implicit def eq[T: Eq, P]: Eq[OpaqueId[T, P]] = Eq by (_.id)
  }

  trait OpaqueIdC[OIDT <: IdTypeTraits] {
    def apply(v: OIDT#Value) = OpaqueId[OIDT#Value, OIDT#Phantom](v)
    def fresh: OIDT          = ???
  }

  object LongId {
    def reserved[P] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = OpaqueId[Int, P](Int.MinValue)
  }

}
