package io.deftrade
package money

import io.deftrade.implicits._

import eu.timepit.refined
import refined.api.{ RefType, Validate }

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
  *   type  Money[N, C] = Refined[N, C]
  * }}}
  *
  * and be done?
  *
  * Because we'd like to add the usual operators directly and without overhead, the use of the
  * default `Refined` type is precluded: root cause being that a value class cannot wrap another
  * value class.
  *
  * Therefore we make use of the `RefType` and `Validated` classes to integrate with `Refined`.
  */
final class Money[N, C] private (val amount: N) extends AnyVal with Serializable { lhs =>

  import spire.implicits._
  import Money.fiat

  def +(rhs: Money[N, C])(implicit N: Financial[N]): Money[N, C] = lhs.amount + rhs.amount |> fiat

  def -(rhs: Money[N, C])(implicit N: Financial[N]): Money[N, C] = lhs.amount - rhs.amount |> fiat

  def *[S](scale: S)(implicit N: Financial[N], S: Financial[S]): Money[N, C] =
    S.to[N](scale) * amount |> fiat

  def /(rhs: Money[N, C])(implicit N: Financial[N]): N = lhs.amount / rhs.amount

  def unary_-(implicit N: Financial[N]): Money[N, C] = -amount |> fiat

  override def toString: String = amount.toString

}

/**
  * `Money[?, C]: Order: Show: CommutativeGroup`
  */
object Money {

  /** `fiat` is the new `unsafe` ;) */
  private def fiat[N, C](n: N): Money[N, C] = new Money(n)

  def apply[N: Financial, C: Currency](amount: N): Money[N, C] =
    Financial[N].round[C](amount) |> fiat

  /** Unpacks into a `(N, C)`. */
  def unapply[N: Financial, C: Currency](m: Money[N, C]): Option[(N, Currency[C])] =
    (m.amount, Currency[C]).some

  /** */
  implicit def moneyOrder[N: Financial, C]: Order[Money[N, C]] =
    Order by (_.amount)

  /**
    * A Show implementation which uses [[format]].
    *
    * `toString` is limited by the decision to use value class for [[Money]].
    * But we can implement Show[Money[N, C]] for all the N and C we care about.
    * The Currency[_] instance is legit necessary for proper formatting.
    */
  implicit def moneyShow[N: Financial, C: Currency]: Show[Money[N, C]] =
    Show show (m => format(m))

  /** Money is a commutative group under addition. */
  implicit def moneyCommutativeGroup[N: Financial, C]: CommutativeGroup[Money[N, C]] =
    Invariant[CommutativeGroup].imap(Financial[N].commutativeGroup)(_ |> fiat[N, C])(_.amount)

  /** Stylized output. */
  def format[N, C](m: Money[N, C])(implicit N: Financial[N], C: Currency[C]): String = {

    // decimal-separator, grouping-separators, parens-for-negative
    def flags = """#,("""

    // TODO: this hack won't extend to alt currency
    def fmt = s"%${flags}.${C.fractionDigits}f"

    def isNegative = N.signum(m.amount) < 0

    def sfmt = if (isNegative) fmt else s" $fmt "

    s"${C.code} ${m.amount formatted sfmt}"
  }

  /** Strictly checked input. */
  def parse[N, C](x: String)(implicit N: Financial[N], C: Currency[C]): Result[Money[N, C]] = {
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

  /**
    * Typeclass instance which integrates [[Money]] with the
    * [[https://github.com/fthomas/refined Refined]] library.
    *
    * TODO why do we need this? The `Validate` mechanism and related inferences
    * do nothing for us. YAGNI?
    */
  implicit lazy val refinedRefType: RefType[Money] =
    new RefType[Money] {

      private type F[T, P] = Money[T, P]

      def unsafeWrap[T, P](t: T): F[T, P] = new Money[T, P](t)

      def unwrap[T](tp: F[T, _]): T = tp.amount

      def unsafeRewrap[T, A, B](ta: F[T, A]): F[T, B] = ta |> unwrap |> unsafeWrap
    }

  /**
    * Valiation policy: Any `C` with a [[Currency]] implicit instance.
    *
    * Design rational(ization?):
    *
    * why not the following, which seems more obvious?
    * {{{
    * implicit object refinedRefType extends RefType[Money] { ... }
    * }}}
    *
    * because:
    *
    * bridge generated for member method unsafeWrap: [T, P](t: T)io.deftrade.money.Money[T,P]
    * in object refinedRefType
    * which overrides method unsafeWrap: [T, P](t: T)F[T,P] in trait RefType
    * clashes with definition of the member itself;
    * both have erased type (t: Object)Object
    *     def unsafeWrap[T, P](t: T): Money[T, P] = new Money(t)
    */
  implicit def refinedValidate[T: Financial, P: Currency]: Validate[T, P] =
    Validate alwaysPassed Currency[P]

}
