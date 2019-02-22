package io.deftrade
package money

import eu.timepit.refined

import spire.implicits._

import cats.kernel.{ CommutativeGroup, Order }
import cats.{ Invariant, Show }
import cats.syntax.option._

/**
  * `Money` is a scala value class, with a phantom currency type.
  *
  * So why can't we just say
  * ```
  *   type  Money[N, C] = Refined[N, C]
  * ```
  *
  * and be done?
  *
  * Because we'd like to add the usual operators directly and without overhead, the use of the
  * default `Refined` type is precluded: root cause being that a value class cannot wrap another
  * value class.
  *
  * Therefore we make use of the `RefType` and `Validated` classes to integrate with `Refined`.
  */
final class Money[N, C] private (val amount: N) extends AnyVal { lhs =>

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

  def unapply[N: Financial, C: Currency](m: Money[N, C]): Option[N] = m.amount.some

  implicit def catsOrderMoney[N: Financial, C: Currency]: Order[Money[N, C]] =
    Order by (_.amount)

  /**
    * `toString` is limited due to value class implementation
    * But we can implement Show[Money[N, C]] for all the N and C we care about.
    */
  implicit def showMoney[N: Financial, C: Currency]: Show[Money[N, C]] =
    Show show (m => format(m))

  implicit def commutativeGroupMoney[N: Financial, C: Currency]: CommutativeGroup[Money[N, C]] =
    Invariant[CommutativeGroup].imap(Financial[N].commutativeGroup)(Currency[C] apply _)(_.amount)

  def format[N: Financial, C: Currency](m: Money[N, C]): String = {
    val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
    val C     = Currency[C]
    val fmt   = s"%${flags}.${C.fractionDigits}f" // TODO: this hack won't extend
    val sfmt  = if ((Financial[N].fractional signum m.amount) < 0) fmt else s" $fmt "
    s"${C.currencyCode} ${m.amount formatted sfmt}"
  }

  /** `Refined` section */
  import refined.api.{ RefType, Validate }
  implicit lazy val refinedRefType: RefType[Money] =
    new RefType[Money] {

      private type F[T, P] = Money[T, P]

      def unsafeWrap[T, P](t: T): F[T, P] = new Money[T, P](t)

      def unwrap[T](tp: F[T, _]): T = tp.amount

      def unsafeRewrap[T, A, B](ta: F[T, A]): F[T, B] = ta |> unwrap |> unsafeWrap
    }

  /**
    * Design rational:
    *
    * why not the following, which seems more obvious?
    * `implicit object refinedRefType extends RefType[Money] {`
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
