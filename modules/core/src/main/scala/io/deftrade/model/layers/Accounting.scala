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
package layers

import money.{ Currency, Financial }, keyval.DtEnum

import cats.implicits._

import enumeratum._

/**
  * Core accounting vocabulary.
  */
trait Accounting { self: ModuleTypes =>

  /** */
  sealed trait AccountingKey extends EnumEntry with Product with Serializable

  /** */
  object AccountingKey {

    /** FIXME whither cats enum?  */
    implicit def orderKeys[K <: AccountingKey]: cats.Order[K] = cats.Order by (_.entryName)

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
  sealed trait Debit extends AccountingKey
  object Debit extends DtEnum[Debit] {

    /** */
    lazy val values = Asset.values ++ Expense.values
  }

  /** */
  sealed trait Credit extends AccountingKey
  object Credit extends DtEnum[Credit] {

    /** */
    lazy val values = Liability.values ++ Revenue.values
  }

  /** */
  trait Liability extends Credit

  /** */
  object Liability extends DtEnum[Liability] {

    /** */
    lazy val values = Debt.values ++ Equity.values
  }

  /** */
  trait Expense extends Debit
  val Expense: DtEnum[_ <: Expense]

  /** */
  trait Revenue extends Credit
  val Revenue: DtEnum[_ <: Revenue]

  /** */
  trait Asset extends Debit
  val Asset: DtEnum[_ <: Asset]

  /** And by income we mean net income. */
  trait Income extends Credit
  val Income: DtEnum[_ <: Income]

  /** Reserved for now. :| */
  trait Reserve extends Credit
  val Reserve: DtEnum[_ <: Reserve]

  /** */
  trait Debt extends Liability
  val Debt: DtEnum[_ <: Debt]

  /** */
  trait Equity extends Liability
  val Equity: DtEnum[_ <: Equity]

  /** instantiate double entry key module with appropriate monetary amount type */
  /** Mapping accounting keys to [[money.Money]]. */
  final type AccountMap[A <: AccountingKey, C] = Map[A, Mny[C]]

  /** */
  object AccountMap {

    /** */
    def empty[K <: AccountingKey, C: Currency]: AccountMap[K, C] = Map.empty
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
  sealed trait Nettable extends EnumEntry with Serializable {

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
  object Nettable {

    /** */
    abstract class Aux[D <: Asset](val gross: D, val less: D) extends Nettable {
      final type AssetType = D
    }
  }

  val Nettables: DtEnum[Nettable]

  /**
    * We call the assingment of fractional amounts to certain accounting keys a ''treatment'',
    * following terminology common in the accounting field.
    */
  type Treatment[K <: AccountingKey] = UnitPartition[K, MonetaryAmount]

  /** */
  final val Treatment = UnitPartition

  /**
    * General form of a "double (bookkeeping) entry" key.
    */
  sealed trait DoubleEntryKey extends EnumEntry with Product with Serializable {

    /** */
    type EntryKey <: AccountingKey

    /** */
    type ContraKey <: AccountingKey

    /** */
    def entries: Treatment[EntryKey]

    /** */
    def contras: Treatment[ContraKey]
  }

  /** */
  object DoubleEntryKey {

    /** subclass which assigns type parameters to type members */
    sealed abstract class Aux[K1 <: AccountingKey, K2 <: AccountingKey](
        es: Treatment[K1],
        cs: Treatment[K2]
    ) extends DoubleEntryKey {

      /** */
      final type EntryKey = K1

      /** */
      final type ContraKey = K2

      /** */
      final def entries: Treatment[EntryKey] = es

      /** */
      final def contras: Treatment[ContraKey] = cs
    }
  }

  /** not sealed - extension point */
  abstract class DebitCreditKey[D <: Debit, C <: Credit](
      debits: Treatment[D],
      credits: Treatment[C]
  ) extends DoubleEntryKey.Aux(debits, credits) {
    final def debits: Treatment[D]  = entries
    final def credits: Treatment[C] = contras
  }

  /** */
  object DebitCreditKey {

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[D <: Debit, C <: Credit, CCY: Currency](
        ks: DebitCreditKey[D, C],
        amount: Mny[CCY]
    ): (AccountMap[D, CCY], AccountMap[C, CCY]) =
      (
        (ks.debits.kvs map (amount * _.value)).toSortedMap,
        (ks.credits.kvs map (amount * _.value)).toSortedMap
      )

    /** */
    def unapply[D <: Debit, C <: Credit](
        dck: DebitCreditKey[D, C]
    ): Option[(Treatment[D], Treatment[C])] =
      (dck.debits, dck.credits).some
  }

  /**
    * Keys that preserve the balance of a [[Balances.BalanceSheet]].
    *
    * `SwapKey`'s type parameter restricts the swap to occur
    * within the same "column" of the `Balance`.
    */
  abstract class SwapKey[K <: AccountingKey] private[model] (
      val from: Treatment[K],
      val to: Treatment[K]
  ) extends DoubleEntryKey.Aux(from, to)

  /** */
  object SwapKey {

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[K <: AccountingKey, C: Currency](
        ks: SwapKey[K],
        amount: Mny[C]
    ): AccountMap[K, C] = {
      def from = (ks.from.kvs map (-amount * _.value)).toSortedMap
      def to   = (ks.to.kvs map (amount * _.value)).toSortedMap
      from |+| to
    }

    /** */
    def unapply[K <: AccountingKey](sk: SwapKey[K]): Option[(Treatment[K], Treatment[K])] =
      (sk.from, sk.to).some
  }

  /** not sealed - extension point */
  abstract class DebitSwapKey[D <: Debit](
      from: Treatment[D],
      to: Treatment[D]
  ) extends SwapKey(from, to)

  /** */
  object DebitSwapKey {

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[D <: Debit, CCY: Currency](
        ks: DebitSwapKey[D],
        amount: Mny[CCY]
    ): (AccountMap[D, CCY], AccountMap[D, CCY]) =
      (
        (ks.from.kvs map (amount * _.value)).toSortedMap,
        (ks.to.kvs map (amount * _.value)).toSortedMap
      )

    /** */
    def unapply[D <: Debit](
        dsk: DebitSwapKey[D]
    ): Option[(Treatment[D], Treatment[D])] =
      (dsk.from, dsk.to).some
  }

  /** */
  abstract class CreditSwapKey[C <: Credit] private[model] (
      f: Treatment[C],
      t: Treatment[C]
  ) extends SwapKey[C](f, t)

  /** */
  object CreditSwapKey {

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def accountMap[C <: Credit, CCY: Currency](
        ks: CreditSwapKey[C],
        amount: Mny[CCY]
    ): (AccountMap[C, CCY], AccountMap[C, CCY]) =
      (
        (ks.from.kvs map (amount * _.value)).toSortedMap,
        (ks.to.kvs map (amount * _.value)).toSortedMap
      )

    /** */
    def unapply[C <: Credit](
        dsk: CreditSwapKey[C]
    ): Option[(Treatment[C], Treatment[C])] =
      (dsk.from, dsk.to).some
  }
}
