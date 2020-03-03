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
package money

import io.deftrade.implicits._

import cats._
import cats.implicits._
import cats.kernel.{ CommutativeGroup, Order }

/**
  * Models an `amount` of [[Currency]] as scala value class, with a phantom currency type.
  *
  * Design rational(ization?):
  *
  * So why can't we just say
  * {{{
  *   type  Mny[N, C] = Refined[N, C]
  * }}}
  *
  * and be done?
  *
  * Because we'd like to add the usual operators directly and (possibly) without overhead,
  * the use of the default `Refined` type is precluded: a value class cannot wrap another
  * value class.
  *
  * Therefore we make use of the `RefType` and `Validated` classes to integrate with `Refined`.
  *
  * TODO: review b/c this is arguably sketchy.
  * OTOH: would be easy to adapt this code to a pure Refined based impl.
  */
final class Mny[N, C] private (val amount: N) extends AnyVal with Serializable { lhs =>

  import spire.implicits._

  def +(rhs: Mny[N, C])(implicit N: Financial[N]): Mny[N, C] = lhs.amount + rhs.amount |> moar

  def -(rhs: Mny[N, C])(implicit N: Financial[N]): Mny[N, C] = lhs.amount - rhs.amount |> moar

  def *[S](scale: S)(implicit N: Financial[N], S: Financial[S]): Mny[N, C] =
    S.to[N](scale) * amount |> moar

  def /(rhs: Mny[N, C])(implicit N: Financial[N]): N = lhs.amount / rhs.amount

  def unary_-(implicit N: Financial[N]): Mny[N, C] = -amount |> moar

  override def toString: String = amount.toString

  @inline private def moar(n: N): Mny[N, C] = new Mny(n)
}

/** */
object Mny {

  /** typeclass instance checks not required because public interface checks first */
  @inline private[money] def fiat[N, C](amount: N): Mny[N, C] =
    new Mny(amount)

  /** */
  def apply[N: Financial, C: Currency](amount: N): Mny[N, C] =
    Mny fiat amount

  /** Unpacks into a `(N, C)`. */
  def unapply[N: Financial, C: Currency](m: Mny[N, C]): Option[(N, Currency[C])] =
    (m.amount, Currency[C]).some

  /** */
  implicit def moneyOrder[N: Financial, C]: Order[Mny[N, C]] =
    Order by (_.amount)

  /**
    * A Show implementation which uses [[format]].
    *
    * `toString` is limited by the decision to use value class for [[Mny]].
    * But we can implement Show[Mny[N, C]] for all the N and C we care about.
    * The Currency[_] instance is legit necessary for proper formatting.
    */
  implicit def moneyShow[N: Financial, C: Currency]: Show[Mny[N, C]] =
    Show show (m => format(m))

  /** Mny is a commutative group under addition. */
  implicit def moneyCommutativeGroup[N: Financial, C: Currency]: CommutativeGroup[Mny[N, C]] =
    Invariant[CommutativeGroup]
      .imap(Financial[N].commutativeGroup)(_ |> Currency[C].apply[N])(_.amount)

  /** Stylized output. */
  def format[N, C](m: Mny[N, C])(implicit N: Financial[N], C: Currency[C]): String = {

    // decimal-separator, grouping-separators, parens-for-negative
    def flags = """#,("""

    // TODO: this hack won't extend to alt currency
    def fmt = s"%${flags}.${C.fractionDigits.toString}f"

    def isNegative = N.signum(m.amount) < 0

    def sfmt = if (isNegative) fmt else s" $fmt "

    s"${C.code.toString} ${m.amount formatted sfmt}"
  }

  /** Strictly checked input. */
  def parse[N, C](x: String)(implicit N: Financial[N], C: Currency[C]): Result[Mny[N, C]] = {
    import spire.syntax.field._
    import N.one
    def ccy  = (x take 3).toUpperCase
    def sign = if ((x charAt 5) === '(') -one else one
    for {
      _ <- if (C.toString === ccy)
            Result.Ok
          else
            Result fail s"expected: ${C.toString} read $ccy"
      amount = x drop 3 + 1 + 1 dropRight 1 + 1
      n <- N parse amount
    } yield C(sign * n)
  }

  // import eu.timepit.refined
  // import refined.api.{ RefType, Validate }
  // /**
  //   * Typeclass instance which integrates [[Mny]] with the
  //   * [[https://github.com/fthomas/refined Refined]] library.
  //   */
  // implicit lazy val refinedRefType: RefType[Mny] =
  //   new RefType[Mny] {
  //
  //     private type F[T, P] = Mny[T, P]
  //
  //     def unsafeWrap[T, P](t: T): F[T, P] = new Mny[T, P](t)
  //
  //     def unwrap[T](tp: F[T, _]): T = tp.amount
  //
  //     def unsafeRewrap[T, A, B](ta: F[T, A]): F[T, B] = ta |> unwrap |> unsafeWrap
  //   }
  //
  // /**
  //   * Valiation policy: Any `C` with a [[Currency]] implicit instance.
  //   *
  //   * Design rational(ization?):
  //   *
  //   * why not the following, which seems more obvious?
  //   * {{{
  //   * implicit object refinedRefType extends RefType[Mny] { ... }
  //   * }}}
  //   *
  //   * because:
  //   *
  //   * bridge generated for member method unsafeWrap: [T, P](t: T)io.deftrade.money.Mny[T,P]
  //   * in object refinedRefType
  //   * which overrides method unsafeWrap: [T, P](t: T)F[T,P] in trait RefType
  //   * clashes with definition of the member itself;
  //   * both have erased type (t: Object)Object
  //   *     def unsafeWrap[T, P](t: T): Mny[T, P] = new Mny(t)
  //   */
  // implicit def refinedValidate[T: Financial, P: Currency]: Validate[T, P] =
  //   Validate alwaysPassed Currency[P]
}
