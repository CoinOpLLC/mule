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

import money.{ Currency, Financial, UnitPartition }, keyval.DtEnum

import cats.implicits._
import cats.{ Order => Oardur }

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
    implicit def orderKeys[K <: AccountingKey]: Oardur[K] = Oardur by (_.entryName)
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

  /** */
  trait Income extends Debit
  val Income: DtEnum[_ <: Income]

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
  abstract class Nettable[D <: Asset](val gross: D, val less: D) extends NettableLike {
    final type AssetType = D
  }

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
    type EntryType <: AccountingKey

    /** */
    type ContraType <: AccountingKey

    /** */
    def entries: Treatment[EntryType]

    /** */
    def contras: Treatment[ContraType]
  }

  /** placeholder */
  object DoubleEntryKey {

    /** */
    sealed abstract class Aux[ENTRY <: AccountingKey, CONTRA <: AccountingKey](
        es: Treatment[ENTRY],
        cs: Treatment[CONTRA]
    ) extends DoubleEntryKey {

      /** */
      final type EntryType = ENTRY

      /** */
      final type ContraType = CONTRA

      /** */
      final def entries: Treatment[EntryType] = es

      /** */
      final def contras: Treatment[ContraType] = cs
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

  /** placeholder */
  object DebitCreditKey

  /**
    * Keys that preserve the balance of a [[Balances.BalanceSheet]].
    *
    * `SwapKey`'s type parameter restricts the swap to occur
    * within the same "column" of the `Balance`.
    */
  abstract class SwapKey[T <: AccountingKey] private[model] (
      val from: Treatment[T],
      val to: Treatment[T]
  ) extends DoubleEntryKey.Aux(from, to)

  /** */
  object SwapKey {

    /** */
    def accountMap[A <: AccountingKey, C: Currency](
        ks: SwapKey[A],
        amount: Mny[C]
    ): AccountMap[A, C] = {
      def from = ks.from.toSortedMap mapValues (-amount * _)
      def to   = ks.to.toSortedMap mapValues (amount * _)
      from |+| to
    }

    /** */
    def unapply[E <: AccountingKey](sk: SwapKey[E]): Option[(Treatment[E], Treatment[E])] =
      (sk.from, sk.to).some
  }

  /** */
  abstract class SimpleDebitCreditKey(
      final val debit: Debit,
      final val credit: Credit
  ) extends DebitCreditKey(Treatment single debit, Treatment single credit)

  /** */
  val SimpleDebitCreditKey: DtEnum[_ <: SimpleDebitCreditKey]
}
