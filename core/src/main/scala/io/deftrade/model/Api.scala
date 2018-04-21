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
  object Quantity {

    /**  Stable values useful for pattern matching. */
    val Zero: Quantity = Financial[Quantity].zero

    /**  Stable values useful for pattern matching. */
    val One: Quantity = Financial[Quantity].one

    /** Extractors and (any) other tools for dealing with integral quantities. */
    object Integral {

      def unapply[N: Integral](q: Quantity): Option[N] = ???
      // TODO: etc
    }
  }

  sealed trait Role extends EnumEntry
  object Role extends Enum[Role] {

    /**
      * Semantics for `Principle` are conditioned on the status of account:
      * - responsible party (liability)
      * - beneficial owner (asset)
      */
    case object Principle extends Role

    /**
      * The primary delegate selected by a `Principle`.
      * Also, simply, the entity(s) whose names are listed on the `Account`,
      * and the primary point of contact for the `Account`.
      * By convention a `Princple` is their own `Agent` unless otherwise specified.
      */
    case object Agent extends Role

    /**
      * The primary delegate selected by the `Agent`.
      * `Entity`(s) with responsibility for, and authority over,
      * the disposition of assets in the `Account`.
      *
      * In particular, `Manager`s may intitiate `Transaction`s which will settle to the `Ledger`.
      *
      * By convention an `Agent` is their own `Manager` unless otherwise specified.
      */
    case object Manager extends Role

    /**
      * `Regulator`s are first class entities, each with a package of rights and responsibilities
      * which is juristiction specific.
      * Practically, what this means is that `Regulator`s will have a (possibly limited) view
      * into the state of the `Ledger`,
      * and (possibly) the ability to block the settlement of `Transaction`s to the `Ledger`
      * or even intitiate `Transaction`s.
      */
    case object Regulator extends Role

    /** The `findValues` macro collects all `value`s in the order written. */
    lazy val values: IndexedSeq[Role] = findValues
  }

  trait Person
  trait Corporation

  type Entity   = Either[Corporation, Person]
  type EntityId = Long Refined Interval.Closed[W.`100000100100`, W.`999999999999`]
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

  type Position = (SecurityId, Quantity)    // '`Position` := `Trade` at rest
  type Folio    = Map[SecurityId, Quantity] // n.b algebraic relationship Position <=> Folio

  object Folio {
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  type FolioId = OpaqueId[Long, Folio]
  implicit def FolioIdHasEq = Eq.fromUniversalEquals[FolioId]

  type Leg   = Position // `Leg` := `Position` in motion
  type Trade = Folio    // `Trade` := `Folio` in motion
  val Trade = Folio // gives you Trade(legs: Leg*): Trade

  def quoteLeg[CCY: Monetary](market: Market)(leg: Leg): Money[MonetaryAmount, CCY] =
    leg match {
      case (security, quantity) => Market.quote[CCY](market)(security) * quantity
    }

  def quote[CCY: Monetary](market: Market)(trade: Trade): Money[MonetaryAmount, CCY] =
    trade.toList foldMap quoteLeg[CCY](market)

  /**
    * The archetypical
    */
  type Portion = (EntityId, Quantity)
  object Portion {
    lazy val Q = Financial[Quantity]
    import Q._
    def validate(p: Portion): Boolean = {
      val (_, q) = p
      Q.zero <= q && q <= Q.one
    }
    def apply(eid: EntityId, q: Quantity): Portion = (eid, q) ensuring { p =>
      validate(p)
    }
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
    lazy val Q = Financial[Quantity]
    def verify(p: Partition): Boolean =
      p.values.toList.sum == Q.one // FIXME this should be ===

    def apply(shares: Portion*): Partition = shares.toMap ensuring (p => verify(p))
  }

  type Roster = Map[Role, Partition]
  object Roster {
    def empty: Roster = Map.empty
  }

  // TODO: is it possible to specialize Map akin to NonEmptyList such that it always
  // contains at least an owner?

  // type Account = Map[FolioId, (Folio, Roster)]

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

  type Ledger = Seq[LedgerEntry] // if you must - but reserving the name is a good idea regardless
  object Ledger {}

  type LedgerId = OpaqueId[Long, LedgerKey Or LedgerState]

  type Allocation = Map[FolioId, Quantity] // must sum to 1... TODO typesafe normalization?
  object Allocation {

    lazy val Q = Financial[Quantity]
    import Q._

    def allocate(allocation: Allocation)(order: Order): AllocatedOrder = ???

    def pariPassu(fids: Set[FolioId]): Allocation = {
      val n = fids.size
      (fids.toList zip List.fill(n)(Quantity.One / (Q fromInt n))).toMap
    }
    def proRata(totalShares: Long)(capTable: Map[FolioId, Long]): Allocation =
      capTable map {
        case (folioId, shares) =>
          val mySlice  = Q fromLong shares
          val wholePie = Q fromLong totalShares
          (folioId, mySlice / wholePie)
      }

    object Single {
      def apply(folioId: FolioId): Allocation = Map(folioId -> Quantity.One)
      def unapply(a: Allocation): Option[FolioId] = a.toList match {
        case (folioId, Quantity.One) :: Nil => Some(folioId)
        case _                              => None
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
