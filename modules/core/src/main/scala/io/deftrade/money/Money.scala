package io.deftrade
package money

import eu.timepit.refined
import refined.api.Refined

import spire.math.{ Fractional, Integral }

import enumeratum._

import cats.kernel.{ CommutativeGroup, Order }
import cats.{ Invariant, Show }

import BigDecimal.RoundingMode._

/**
  * `Money` is a scala value class, with a phantom currency type.
  */
final class Money[N, C] private (val amount: N) extends AnyVal { lhs =>

  type MNY = Money[N, C]

  def +(rhs: MNY)(implicit N: Financial[N]) =
    new Money[N, C](N.fractional plus (amount, rhs.amount))

  def -(rhs: MNY)(implicit N: Financial[N]) =
    new Money[N, C](N.fractional minus (amount, rhs.amount))

  def *[S](scale: S)(implicit N: Financial[N], S: Financial[S], C: Currency[C]) =
    Money[N, C](N.fractional times (S to [N] scale, amount))

  def /(rhs: MNY)(implicit N: Financial[N]): N = N.fractional div (lhs.amount, rhs.amount)

  def unary_-(implicit N: Financial[N], C: Currency[C]): MNY = Money(N.fractional negate amount)

  override def toString: String = amount.toString
}

/**
  * `Money[?, C]: Order: Show: CommutativeGroup`
  */
object Money {

  def apply[N: Financial, C: Currency](amount: N) = new Money[N, C](Financial[N].round[C](amount))

  implicit def orderMoney[N: Financial, C: Currency]: Order[Money[N, C]] =
    Order by (_.amount)

  // toString is limited due to value class implementation
  // But we can implement Show[Money[N, C]] for all the N and C we care about.
  implicit def showMoney[N: Financial, C: Currency]: Show[Money[N, C]] =
    Show show (Format apply _)

  implicit def commutativeGroupMoney[N: Financial, C: Currency]: CommutativeGroup[Money[N, C]] =
    Invariant[CommutativeGroup].imap(Financial[N].commutativeGroup)(Currency[C] apply _)(_.amount)

  object Format {
    private val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
    def apply[N: Financial, C: Currency](m: Money[N, C]): String = {
      val C    = Currency[C]
      val fmt  = s"%${flags}.${C.fractionDigits}f" // TODO: this hack won't extend
      val sfmt = if ((Financial[N].fractional signum m.amount) < 0) fmt else s" $fmt "
      s"${C.currencyCode} ${m.amount formatted sfmt}"
    }
  }

  /** section */
  import refined.api.{ RefType, Validate }
  implicit lazy val refinedRefType: RefType[Money] =
    new RefType[Money] {

      private type F[T, P] = Money[T, P]

      def unsafeWrap[T, P](t: T): F[T, P] = new Money[T, P](t)

      def unwrap[T](tp: F[T, _]): T = tp.amount

      def unsafeRewrap[T, A, B](ta: F[T, A]): F[T, B] = ta |> unwrap |> unsafeWrap
    }

  implicit def refinedValidate[T: Financial, P: Currency]: Validate[T, P] =
    Validate alwaysPassed Currency[P]
}
