package io.deftrade
package money

import eu.timepit.refined
import refined.api.{ RefType, Validate }

import cats._
import cats.kernel.{ CommutativeGroup, Order }
import cats.implicits._

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
  * FIXME: turns out not. Delete comment?
  */
final class Money[N, C] private (val amount: N) extends AnyVal { lhs =>

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

  /** TODO: should both amount and currency be extracted here?... */
  def unapply[N: Financial, C: Currency](m: Money[N, C]): Option[N] = m.amount.some

  /** Policy: algebras should not have arbitrary and domain restrictions on the phantom types. */
  implicit def moneyOrder[N: Financial, C]: Order[Money[N, C]] =
    Order by (_.amount)

  /**
    * `toString` is limited due to value class implementation
    * But we can implement Show[Money[N, C]] for all the N and C we care about.
    * The Currency[_] instance is legit necessary for proper formatting.
    */
  implicit def moneyShow[N: Financial, C: Currency]: Show[Money[N, C]] =
    Show show (m => format(m))

  /** Policy: algebras should not have arbitrary and domain restrictions on the phantom types. */
  implicit def moneyCommutativeGroup[N: Financial, C]: CommutativeGroup[Money[N, C]] =
    Invariant[CommutativeGroup].imap(Financial[N].commutativeGroup)(_ |> fiat[N, C])(_.amount)

  def format[N, C](m: Money[N, C])(implicit N: Financial[N], C: Currency[C]): String = {

    // decimal-separator, grouping-separators, parens-for-negative
    def flags = """#,("""

    // TODO: this hack won't extend to alt currency
    def fmt = s"%${flags}.${C.fractionDigits}f"

    def isNegative = N.fractional.signum(m.amount) < 0

    def sfmt = if (isNegative) fmt else s" $fmt "

    s"${C.currencyCode} ${m.amount formatted sfmt}"
  }

  import io.chrisdavenport.cormorant._
  import io.chrisdavenport.cormorant.implicits._

  /** cormorant csv Get */
  implicit def moneyGet[N: Financial, CCY: Currency]: Get[Money[N, CCY]] = new Get[Money[N, CCY]] {

    val CCY = Currency[CCY]

    def get(field: CSV.Field): Either[Error.DecodeFailure, Money[N, CCY]] = {
      import field.x
      import spire.syntax.field._
      val one    = Financial[N].fractional.one
      def sign   = if (x.charAt(5) === '(') -one else one
      val ccy    = (x take 3) |> CSV.Field.apply
      val amount = (x drop 3 + 1 + 1 dropRight 1 + 1) |> CSV.Field.apply
      for {
        _ <- Get[Currency[CCY]] get ccy
        n <- Get[N] get amount
      } yield CCY apply sign * n
    }
  }

  /** cormorant csv Put */
  implicit def moneyPut[N: Financial, C: Currency]: Put[Money[N, C]] =
    stringPut contramap (m => format(m))

  /** `Refined` section HACK  why do we need this? */
  // implicit lazy val refinedRefType: RefType[Money] =
  //   new RefType[Money] {
  //
  //     private type F[T, P] = Money[T, P]
  //
  //     def unsafeWrap[T, P](t: T): F[T, P] = new Money[T, P](t)
  //
  //     def unwrap[T](tp: F[T, _]): T = tp.amount
  //
  //     def unsafeRewrap[T, A, B](ta: F[T, A]): F[T, B] = ta |> unwrap |> unsafeWrap
  //   }
  //
  // /**
  //   * Design rational(ization?):
  //   *
  //   * why not the following, which seems more obvious?
  //   * `implicit object refinedRefType extends RefType[Money] {`
  //   *
  //   * because:
  //   *
  //   * bridge generated for member method unsafeWrap: [T, P](t: T)io.deftrade.money.Money[T,P]
  //   * in object refinedRefType
  //   * which overrides method unsafeWrap: [T, P](t: T)F[T,P] in trait RefType
  //   * clashes with definition of the member itself;
  //   * both have erased type (t: Object)Object
  //   *     def unsafeWrap[T, P](t: T): Money[T, P] = new Money(t)
  //   */
  // implicit def refinedValidate[T: Financial, P: Currency]: Validate[T, P] =
  //   Validate alwaysPassed Currency[P]
}
