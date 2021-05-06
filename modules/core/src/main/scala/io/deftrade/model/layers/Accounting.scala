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
package model.layers

import money.{ Currency, Financial }
import keyval.DtEnum
import model.slices.{ UnitPartio }

import cats.implicits._

import enumeratum.EnumEntry

/**
  * Core accounting vocabulary.
  */
trait Accounting { self: ModuleTypes =>

  /**
    */
  sealed trait AccountingKey extends EnumEntry

  /**
    */
  object AccountingKey {

    /**
      */
    implicit def orderKeys[K <: AccountingKey]: cats.Order[K] = cats.Order by (_.entryName)

    /**
      */
    object syntax {

      /**
        */
      implicit final class AccountingMapOps[K <: AccountingKey, V](m: Map[K, V]) {

        /** TODO: understand if both of these `collect` methods are needed
          */
        def collect[L <: K](pf: PartialFunction[K, L]): Map[L, V] =
          collectKeys(pf.lift)

        /**
          * Filters a map by narrowing the scope of the keys contained.
          *
          * TODO: Revisit. Feels awkward, but DRY and reflection free. Type bounds and pattern
          * summation check each other.
          *
          * @param subKey Easily provided via an extractor.
          * @return A map containing those entries whose keys match a subclassing pattern.
          * @see [[keyval.DtEnum]]
          */
        def collectKeys[L <: K](subKey: K => Option[L]): Map[L, V] =
          m collect (Function unlift { case (k, v) => subKey(k) map { _ -> v } })

        /**
          */
        def widenKeys[J >: K <: AccountingKey]: Map[J, V] = m.toMap // shrugs

        /**
          */
        def denominated[C: Currency](implicit V: Financial[V]): AccountingMap[K, C] =
          m map {
            case (k, n) =>
              (k, Currency[C] apply (Financial[V] to [MonetaryAmount] n))
          }
      }
    }
  }

  /**
    */
  sealed trait Debit extends AccountingKey
  object Debit extends DtEnum[Debit] {

    /**
      */
    lazy val values = Asset.values ++ Expense.values
  }

  /**
    */
  sealed trait Credit extends AccountingKey
  object Credit extends DtEnum[Credit] {

    /**
      */
    lazy val values = Liability.values ++ Revenue.values
  }

  /**
    */
  trait Liability extends Credit

  /**
    */
  object Liability extends DtEnum[Liability] {

    /**
      */
    lazy val values = Debt.values ++ Equity.values
  }

  /**
    */
  trait Expense extends Debit
  val Expense: DtEnum[_ <: Expense]

  /**
    */
  trait Revenue extends Credit
  val Revenue: DtEnum[_ <: Revenue]

  /**
    */
  trait Asset extends Debit
  val Asset: DtEnum[_ <: Asset]

  /** And by income we mean net income.
    */
  trait Income extends Credit
  val Income: DtEnum[_ <: Income]

  /** Reserved for now. :|
    */
  trait Reserve extends Credit
  val Reserve: DtEnum[_ <: Reserve]

  /**
    */
  trait Debt extends Liability
  val Debt: DtEnum[_ <: Debt]

  /**
    */
  trait Equity extends Liability
  val Equity: DtEnum[_ <: Equity]

  /** Legal tender, immediately available funds (payable on demand).
    */
  sealed trait Cash extends Asset
  object Cash extends DtEnum[Cash] {
    case object Physical   extends Cash
    case object Deposit    extends Cash
    case object StableCoin extends Cash
    lazy val values = findValues
  }

  /** instantiate double entry key module with appropriate monetary amount type */
  /** Mapping accounting keys to [[money.Mny]]. */
  final type AccountingMap[A <: AccountingKey, C] = Map[A, Money[C]]

  /**
    */
  object AccountingMap {

    /**
      */
    def empty[K <: AccountingKey, C: Currency]: AccountingMap[K, C] = Map.empty
  }

  final type Debits[C]  = AccountingMap[Debit, C]
  final type Credits[C] = AccountingMap[Credit, C]

  final type Assets[C]      = AccountingMap[Asset, C]     // per BalanceSheet
  final type Liabilities[C] = AccountingMap[Liability, C] // per BalanceSheet
  final type Equities[C]    = AccountingMap[Equity, C]    // Assets net of Liabilities
  final type Revenues[C]    = AccountingMap[Revenue, C]   // top line
  final type Expenses[C]    = AccountingMap[Expense, C]   //
  final type Incomes[C]     = AccountingMap[Income, C]    // bottom line
  final type Cashes[C]      = AccountingMap[Cash, C]      // TODO: name needs work

  /**
    */
  sealed trait Nettable extends EnumEntry with Serializable {

    /**
      */
    type AssetType <: Asset

    /**
      */
    def gross: AssetType

    /**
      */
    def less: AssetType

    /**
      */
    final def net[C: Currency](assets: Assets[C]): Money[C] =
      assets(gross) - assets(less)
  }

  /**
    */
  object Nettable {

    /**
      */
    abstract class Aux[DB <: Asset](val gross: DB, val less: DB) extends Nettable {
      final type AssetType = DB
    }
  }

  val Nettables: DtEnum[Nettable]

  /**
    * We call the assingment of fractional amounts to certain accounting keys a ''treatment'',
    * following terminology common in the accounting field.
    */
  type Treatment[K <: AccountingKey] = UnitPartio[K, MonetaryAmount]

  /**
    */
  final val Treatment = UnitPartio

  /**
    * General form of a "double (bookkeeping) entry" key.
    */
  sealed trait DoubleEntryKey extends EnumEntry {

    /**
      */
    type EntryKey <: AccountingKey

    /**
      */
    type ContraKey <: AccountingKey

    /**
      */
    def entries: Treatment[EntryKey]

    /**
      */
    def contras: Treatment[ContraKey]
  }

  /**
    */
  object DoubleEntryKey {

    /** subclass which assigns type parameters to type members */
    sealed abstract class Aux[
        K1 <: AccountingKey,
        K2 <: AccountingKey
    ](
        es: Treatment[K1],
        cs: Treatment[K2]
    ) extends DoubleEntryKey {

      /**
        */
      final type EntryKey = K1

      /**
        */
      final type ContraKey = K2

      /**
        */
      final def entries: Treatment[EntryKey] = es

      /**
        */
      final def contras: Treatment[ContraKey] = cs
    }
  }

  /** not sealed - extension point */
  abstract class DebitCreditKey(
      final val debits: Treatment[Debit],
      final val credits: Treatment[Credit]
  ) extends DoubleEntryKey.Aux(debits, credits)

  /**
    */
  object DebitCreditKey {

    /**
      */
    private[model] def apply(debits: Treatment[Debit], credits: Treatment[Credit]) =
      new DebitCreditKey(debits, credits) {}

    /**
      */
    def unapply(dc: DebitCreditKey): Option[(Treatment[Debit], Treatment[Credit])] =
      (dc.debits, dc.credits).some

    /**
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountingMaps[C: Currency](
        dc: DebitCreditKey,
        amount: Money[C]
    ): (AccountingMap[Debit, C], AccountingMap[Credit, C]) =
      (
        (dc.debits.kvs map (amount * _.value)).toSortedMap,
        (dc.credits.kvs map (amount * _.value)).toSortedMap
      )
  }

  /**
    * Keys that preserve the balance of a [[Balances.Balance]].
    *
    * `SwapKey`'s type parameter restricts the swap to occur
    * within the same "column" of the `Balance`.
    */
  sealed abstract class SwapKey[K <: AccountingKey] private[model] (
      val from: Treatment[K],
      val to: Treatment[K]
  ) extends DoubleEntryKey.Aux(from, to)

  /**
    */
  object SwapKey {

    /**
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[K <: AccountingKey, C: Currency](
        s: SwapKey[K],
        amount: Money[C]
    ): AccountingMap[K, C] = {
      def from = (s.from.kvs map (-amount * _.value)).toSortedMap
      def to   = (s.to.kvs map (amount * _.value)).toSortedMap
      from |+| to
    }

    /**
      */
    def unapply[K <: AccountingKey](sk: SwapKey[K]): Option[(Treatment[K], Treatment[K])] =
      (sk.from, sk.to).some
  }

  /** not sealed - extension point
    */
  abstract class DebitSwapKey(
      from: Treatment[Debit],
      to: Treatment[Debit]
  ) extends SwapKey(from, to)

  /**
    */
  object DebitSwapKey {

    /**
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[C: Currency](
        ks: DebitSwapKey,
        amount: Money[C]
    ): (AccountingMap[Debit, C], AccountingMap[Debit, C]) =
      (
        (ks.from.kvs map (amount * _.value)).toSortedMap,
        (ks.to.kvs map (amount * _.value)).toSortedMap
      )

    /**
      */
    def unapply(
        ds: DebitSwapKey
    ): Option[(Treatment[Debit], Treatment[Debit])] =
      (ds.from, ds.to).some
  }

  /**
    */
  abstract class CreditSwapKey private[model] (
      f: Treatment[Credit],
      t: Treatment[Credit]
  ) extends SwapKey[Credit](f, t)

  /**
    */
  object CreditSwapKey {

    /**
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[C: Currency](
        ks: CreditSwapKey,
        amount: Money[C]
    ): (AccountingMap[Credit, C], AccountingMap[Credit, C]) =
      (
        (ks.from.kvs map (amount * _.value)).toSortedMap,
        (ks.to.kvs map (amount * _.value)).toSortedMap
      )

    /**
      */
    def unapply(
        cs: CreditSwapKey
    ): Option[(Treatment[Credit], Treatment[Credit])] =
      (cs.from, cs.to).some
  }
}
