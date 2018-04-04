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
      * The the entity(s) whose names are listed on the account.
      */
    case object Principle extends Role

    /**
      * Person(s) responsible for the disposition of assets in the Folio.
      */
    case object Manager extends Role

    /**
      * Semantics for `Subject` are conditioned on the status of account:
      * - responsible party (liability)
      * - beneficial owner (asset)
      */
    case object Subject extends Role

    /**
      * `Regulator`s are first class entities.
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

    case class Swaption(sid: SymbolId)                extends Security with Derivative
    case class Swap(sid: SymbolId)                    extends Security with Derivative
    case class FRA(sid: SymbolId)                     extends Security with Derivative
    case class EquityIndexFutureOption(sid: SymbolId) extends Security with Derivative
    case class EquityIndexOption(sid: SymbolId)       extends Security with Derivative
    case class EquityIndexFuture(sid: SymbolId)       extends Security with Derivative
    case class EquityVanillaOption(sid: SymbolId)     extends Security with Derivative
    case class FxSwap(sid: SymbolId)                  extends Security with Derivative
    case class FxVanillaOption(sid: SymbolId)         extends Security with Derivative
    case class FxForwardSpot(sid: SymbolId)           extends Security
    case class EquityCommon(sid: SymbolId)            extends Security
    case class EquityPreferred(sid: SymbolId)         extends Security
    case class DebtConvertible(sid: SymbolId)         extends Security
    case class DebtBond(sid: SymbolId)                extends Security
    case class DebtAmortizing(sid: SymbolId)          extends Security // Annuity?
    case class TermDeposit(sid: SymbolId)             extends Security // DebtSimple?
    case class BulletPayment(sid: SymbolId)           extends Security

    lazy val values = findValues
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
    * Note that Roster encodes the notion of a `CapitalKey` – per `Role`!
    * The meaning of this `ProRata` will depend upon the role.
    * For the beneficial owner(s) of the assets in question the meaning is obvious.
    * For a `Manager`, the `ProRata` could encode their voting rights – which may differ from
    * their ownership rights.
    */
  type ProRata = Map[EntityId, Quantity]
  object ProRata {}

  type Roster = Map[Role, ProRata]
  // type Roster = Map[Role, Set[EntityId]]
  object Roster {
    def empty: Roster = Map.empty
  }

  type Account = Map[FolioId, (Folio, Roster)]
  object Account {
    def updatedProRata(a: Account, newProRata: Map[EntityId, Quantity]): Account = ???
  }

  type AccountId = OpaqueId[Long, Account]
  implicit def AccountIdHasEq = Eq.fromUniversalEquals[AccountId]

  /**
    * I know what you're thinking: There's only one Ledger™. But consider different views, with
    * different privileges to see different things.
    */
  type Ledger = Map[AccountId, Account]
  object Ledger {

    // TODO: this seems to fix a divirging implicit expansion for Monoid[Account] in `foldMap` - wut
    implicit val FolioIsMonoid   = Monoid[Folio]
    implicit val ProRataIsMonoid = Monoid[ProRata]

    def recorded(ledger: Ledger)(ao: AllocatedOrder): Ledger = ao match {
      case (accountId, folioMap) =>
        folioMap.toList foldMap {
          case (folioId, tradeFolio) =>
            (ledger(accountId) get folioId).fold(
              ledger + (accountId -> Map(folioId -> ((tradeFolio, Roster.empty))))
            ) {
              case (folio, roster) =>
                val updatedPositions = ((folio |+| tradeFolio), roster)
                ledger + (accountId -> Map(folioId -> updatedPositions))
            }
        }
    }
  }
  type LedgerId = OpaqueId[Long, Ledger]

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

  type Order   = (AccountId, Folio)
  type OrderId = OpaqueId[Long, Order]
  object Order {
    import io.deftrade.money.Monetary.USD
    def legacy(bd: BigDecimal, q: Long): PricedTrade[USD] =
      (
        Map(LongId.reserved -> (Financial[Quantity] fromBigDecimal BigDecimal(q))),
        USD(Financial[MonetaryAmount] fromBigDecimal bd)
      )
  }

  type AllocatedOrder = (AccountId, Map[FolioId, Folio])

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
