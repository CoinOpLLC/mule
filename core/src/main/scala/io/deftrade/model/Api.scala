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

import scala.language.higherKinds

import impl._

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  */
abstract class Api[MonetaryAmount: Financial, Quantity: Financial] {

  /** Domain specific tools for dealing with `Quantity`s */
  val Quantity = Financial[Quantity]

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  val MonetaryAmount = Financial[MonetaryAmount]

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

  trait Person
  trait Corporation

  type Entity   = Either[Corporation, Person]
  type EntityId = Long Refined Interval.Closed[W.`100000100100`, W.`999999999999`]
  object EntityId {
    def next: EntityId = ???
  }
  // type EntityId = OpaqueId[Long, Entity]
  implicit def EntityIddHasEq = Eq.fromUniversalEquals[EntityId]

  sealed trait Derivative { self: Security =>
    def underlyer: Security = ???
    def delta: BigDecimal   = ???
  }

  sealed trait SymbolId extends EnumEntry { def id: String }

  object SymbolId extends Enum[SymbolId] {

    case class Cusip(val id: String)        extends SymbolId
    case class Isin(val id: String)         extends SymbolId
    case class Ric(val id: String)          extends SymbolId
    case class Buid(val id: String)         extends SymbolId
    case class IbContractId(val id: String) extends SymbolId
    case class CounterId(val id: String)    extends SymbolId

    lazy val values = findValues
  }

  // TODO: use the XBRL definitions for these
  sealed trait Security extends EnumEntry
  object Security extends Enum[Security] {

    lazy val values = findValues

    // A FixedCouponBond or CapitalIndexedBond.
    case class Bond(val sid: SymbolId) extends Security
    // A BondFuture.
    case class BondFuture(val sid: SymbolId) extends Security
    // A BondFutureOption.
    case class BondFutureOption(val sid: SymbolId) extends Security
    // A BulletPayment.
    case class BulletPayment(val sid: SymbolId) extends Security
    // A product only used for calibration.
    case class Calibration(val sid: SymbolId) extends Security
    // Credit Default Swap (CDS)
    case class Cds(val sid: SymbolId) extends Security
    // CDS index
    case class CdsIndex(val sid: SymbolId) extends Security
    // Constant Maturity Swap (CMS)
    case class Cms(val sid: SymbolId) extends Security
    // A Dsf.
    case class Dsf(val sid: SymbolId) extends Security
    // Exchange Traded Derivative - Future (ETD)
    case class EtdFuture(val sid: SymbolId) extends Security // FIXME: this conflicts wtih mine...

    // fuckit: need the semantic equivalent of "tags" here... moar phantom types?

    // Exchange Traded Derivative - Option (ETD)
    case class EtdOption(val sid: SymbolId) extends Security
    // Forward Rate Agreement
    case class Fra(val sid: SymbolId) extends Security
    // FX Non-Deliverable Forward
    case class FxNdf(val sid: SymbolId) extends Security
    // A FxSingle.
    case class FxSingle(val sid: SymbolId) extends Security
    // A FxSingleBarrierOption.
    case class FxSingleBarrierOption(val sid: SymbolId) extends Security
    // A FxSwap.
    case class FxSwap(val sid: SymbolId) extends Security
    // A FxVanillaOption.
    case class FxVanillaOption(val sid: SymbolId) extends Security with Derivative
    // A IborCapFloor.
    case class IborCapFloor(val sid: SymbolId) extends Security
    // A IborFuture.
    case class IborFuture(val sid: SymbolId) extends Security
    // A IborFutureOption.
    case class IborFutureOption(val sid: SymbolId) extends Security
    // Another kind of product, details not known.

    // case class Other(val sid: SymbolId) extends Security
    // // A Security, used where the kind of security is not known.
    // case class Security(val sid: SymbolId) extends Security

    case class DebtAmortizing(sid: SymbolId)          extends Security
    case class DebtConvertible(sid: SymbolId)         extends Security
    case class EquityCommon(sid: SymbolId)            extends Security
    case class EquityPreferred(sid: SymbolId)         extends Security
    case class EquityIndexFutureOption(sid: SymbolId) extends Security with Derivative
    case class EquityIndexOption(sid: SymbolId)       extends Security with Derivative
    case class EquityIndexFuture(sid: SymbolId)       extends Security with Derivative
    case class EquityVanillaOption(sid: SymbolId)     extends Security with Derivative
    case class FxForwardSpot(sid: SymbolId)           extends Security // FIXME not sure here

    // // A representation based on sensitivities.
    case class Sensitivities(val sid: SymbolId) extends Security
    // A Swap.
    case class Swap(val sid: SymbolId) extends Security
    // A Swaption.
    case class Swaption(val sid: SymbolId) extends Security
    // A TermDeposit.
    case class TermDeposit(val sid: SymbolId) extends Security
  }

  type SecurityId = OpaqueId[Long, Security]
  implicit def AssetIdHasEq = Eq.fromUniversalEquals[SecurityId]

  type Position = (SecurityId, Quantity) // '`Position` := `Trade` at rest
  object Position

  type Folio = Map[SecurityId, Quantity] // n.b algebraic relationship Position <=> Folio
  object Folio {
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  type FolioId = OpaqueId[Long, Folio]
  implicit def FolioIdHasEq = Eq.fromUniversalEquals[FolioId]

  type Leg = Position // `Leg` := `Position` in motion
  val Leg = Position

  type Trade = Folio // `Trade` := `Folio` in motion
  val Trade = Folio // gives you Trade(legs: Leg*): Trade

  def quoteLeg[CCY: Monetary](market: Market)(leg: Leg): Money[MonetaryAmount, CCY] =
    leg match {
      case (security, quantity) => Market.quote[CCY](market)(security) * quantity
    }

  def quote[CCY: Monetary](market: Market)(trade: Trade): Money[MonetaryAmount, CCY] =
    trade.toList foldMap quoteLeg[CCY](market)

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

  type LedgerState = Map[FolioId, Folio]
  object LedgerState

  type Account = Map[FolioId, Roster]
  object Account {
    def updatedProRata(a: Account, newProRata: Map[EntityId, Quantity]): Account = ???
  }

  type AccountId = OpaqueId[Long, Account]
  implicit def AccountIdHasEq = Eq.fromUniversalEquals[AccountId]

  /**
    * (Runtime) invariant: `Trade`s must balance across all the `FolioId`s in the
    * `LedgerEntry`.
    */
  type LedgerEntry = Map[FolioId, Trade]
  object LedgerEntry

  /**
    * What is the `Ledger`, anyway? It's a `Log` structure, essentially – Foldable[LedgerEntry]
    */
  type LedgerKey = Map[AccountId, Account]
  object LedgerKey {

    // TODO: this seems to fix a divirging implicit expansion for Monoid[Account] in `foldMap` - wut
    implicit val FolioIsMonoid     = Monoid[Folio]
    implicit val PartitionIsMonoid = Monoid[Partition]

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

  type Ledger = List[LedgerEntry] // if you must - but reserving the name is a good idea regardless
  object Ledger {}

  type LedgerId = OpaqueId[Long, LedgerKey Or LedgerState]

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

  type Transaction = LedgerEntry
  object Transaction

  type Order   = (AccountId, Trade)
  type OrderId = OpaqueId[Long, Order]
  object Order {
    import io.deftrade.money.Monetary.USD
    def legacy(bd: BigDecimal, q: Long): PricedTrade[USD] =
      (
        Map(LongId.reserved -> (Financial[Quantity] fromBigDecimal BigDecimal(q))),
        USD(Financial[MonetaryAmount] fromBigDecimal bd)
      )
  }

  type AllocatedOrder = (AccountId, Transaction)

  // TODO: type constructors all the way?
  // how to capture the fact that `[CCY: Monetary]`
  type Execution[CCY]   = (OrderId, LocalDateTime, PricedTrade[CCY])
  type ExecutionId[CCY] = OpaqueId[Long, Execution[CCY]]

  type PricedTrade[CCY] = (Trade, Money[MonetaryAmount, CCY])

  sealed trait Exchange // a multiparty nexus
  sealed trait Counter  // a single counterparty or market maker

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

  type EntityRoles  = Map[EntityId, Map[AccountId, Set[Role]]]
  type AccountRoles = Map[AccountId, Map[EntityId, Set[Role]]]

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

object impl {

  private[model] sealed trait Id[T] extends Any { def id: T }

  final case class OpaqueId[T, P](val id: T) extends AnyVal with Id[T]

  object OpaqueId {
    implicit def eq[T: Eq, P]: Eq[OpaqueId[T, P]] = Eq by (_.id)
  }

  object LongId {
    def reserved[P] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = OpaqueId[Int, P](Int.MinValue)
  }

}
