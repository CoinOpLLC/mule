package io.deftrade
package contracts

import spire.math.Fractional
import spire.algebra.Field

import cats.{ Eq, Eval, Group, Monad, Show }
import Eval.later

/**  This trait intentionally left blank.
  */
sealed trait Contract extends Product

/**
  * ADT definitions and constructors form the `Contract` specification DSL.
  *
  * Note internally the `Contract` instances are lazy, via [[cats.Eval]].
  *
  * Implements the [[cats.Group]] typeclass instance for `Contract`.
  */
object Contract {

  final type Oracle[A] = Eval[A]

  implicit lazy val contractEq: Eq[Contract]     = Eq.fromUniversalEquals[Contract]
  implicit lazy val contractShow: Show[Contract] = Show.show[Contract](_.toString)

  case object Zero extends Contract

  sealed abstract case class One private (n: Numéraire) extends Contract
  object One { def apply(n: Numéraire): One = new One(n) {} }

  sealed abstract case class Give private (c: Eval[Contract]) extends Contract
  object Give { def apply(c: Eval[Contract]): Give = new Give(c) {} }

  sealed abstract case class Scale[N] private (
      o: Oracle[N],
      c: Eval[Contract],
      N: Field[N]
  ) extends Contract

  object Scale {
    def apply[N: Field](o: Oracle[N], c: Eval[Contract]): Scale[N] =
      new Scale(o, c, Field[N]) {}
  }

  sealed abstract case class When private (o: Oracle[Boolean], c: Eval[Contract]) extends Contract
  object When { def apply(o: Oracle[Boolean], c: Eval[Contract]): When = new When(o, c) {} }

  sealed abstract case class Until private (o: Oracle[Boolean], c: Eval[Contract]) extends Contract
  object Until { def apply(o: Oracle[Boolean], c: Eval[Contract]): Until = new Until(o, c) {} }

  sealed abstract case class Anytime private (o: Oracle[Boolean], c: Eval[Contract])
      extends Contract
  object Anytime {
    def apply(o: Oracle[Boolean], c: Eval[Contract]): Anytime = new Anytime(o, c) {}
  }

  sealed abstract case class Both private (a: Eval[Contract], b: Eval[Contract]) extends Contract
  object Both { def apply(a: Eval[Contract], b: Eval[Contract]): Both = new Both(a, b) {} }

  sealed abstract case class Pick[F[_]] private (a: Eval[Contract], b: Eval[Contract])
      extends Contract {
    def election: Oracle.Election[F]
    def choice: F[Contract]
  }
  object Pick {
    def apply[F[_]: Monad](t: Eval[Contract], f: Eval[Contract]): Pick[F] =
      new Pick[F](t, f) {
        import cats.implicits._
        final def election: Oracle.Election[F] = ???
        final def choice: F[Contract] =
          for { b <- election.result.value } yield (if (b) t.value else f.value)
      }
  }

  sealed abstract case class Branch private (o: Oracle[Boolean],
                                             t: Eval[Contract],
                                             f: Eval[Contract])
      extends Contract

  object Branch {
    def apply(o: Oracle[Boolean], cT: Eval[Contract], cF: Eval[Contract]): Branch =
      new Branch(o, cT, cF) {}
  }

  /** FIXME: Use `Group[Eval[Contract]]` instead!!!
    */
  implicit lazy val contractGroup: Group[Contract] =
    new Group[Contract] {
      def empty: Contract                             = contracts.zero
      def combine(x: Contract, y: Contract): Contract = contracts.both(x)(y)
      def inverse(a: Contract): Contract              = contracts.give(a)
    }

  /** Tucked in here so `Contract` can stay `sealed` in source file.
    */
  trait primitives {

    /** Party acquires no rights or responsibilities.
      */
    final def zero: Contract =
      Zero

    /** Party immediately acquires one unit of `Numéraire` from counterparty.
      */
    final def unitOf(base: Numéraire): Contract =
      One(base)

    /** Party assumes role of counterparty with respect to `c`.
      */
    final def give(c: => Contract): Contract =
      Give(later(c))

    /** Party acquires `c` multiplied by `n`.
      */
    final def scale[N: Field](n: Oracle[N])(c: => Contract): Contract =
      Scale(n, later(c))

    /** Party will acquire c as soon as `b` is observed `true`.
      */
    final def when(b: Oracle[Boolean])(c: => Contract): Contract =
      When(b, later(c))

    /** Party acquires `c` with the obligation to abandon it when `b` is observed `true`.
      */
    final def until(b: Oracle[Boolean])(c: => Contract): Contract =
      Until(b, later(c))

    /** Party acquires the right (but not the obligation)
      * to acquire `c` at any time the oracle `b` is true.
      */
    final def anytime(b: Oracle[Boolean])(c: => Contract): Contract =
      Anytime(b, later(c))

    /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`.
      */
    final def branch(b: Oracle[Boolean])(t: => Contract)(f: => Contract): Contract =
      Branch(b, later(t), later(f))

    /** Party immediately receives both `cA` and `cB`.
      */
    final def both(a: => Contract)(b: => Contract): Contract =
      Both(later(a), later(b))

    /** Party immediately chooses between `cA` or `cB`.
      */
    final def pick[F[_]: Monad](t: => Contract)(f: => Contract): Contract =
      Pick[F](later(t), later(f))

  }
  implicit class ContractOps(val c: Contract) extends AnyVal {

    def unary_- : Contract =
      contracts.give(c)

    def scaled[N: Field](n: Oracle[N]): Contract =
      contracts.scale(n)(c)

    def *[N: Field](n: Oracle[N]): Contract =
      c scaled n

    def when(b: Oracle[Boolean]): Contract =
      contracts.when(b)(c)

    def anytime(b: Oracle[Boolean]): Contract =
      contracts.anytime(b)(c)

    def until(b: Oracle[Boolean]): Contract =
      contracts.until(b)(c)

    def combine(c2: => Contract): Contract =
      contracts.both(c)(c2)
  }
}

trait api extends Contract.primitives {

  import Numéraire._, Oracle._
  import spire.implicits._

  /** Party acquires one unit of [[money.Currency]].
    */
  def one(c: InCoin): Contract =
    unitOf(c)

  /** Party acquires one unit of ''something'',
    * where that ''something'' (e.g. an [[model.std.Instrument instrument]])
    * is '''non-fungable'''.
    */
  def one(k: InKind): Contract =
    unitOf(k)

  /**
    */
  def allOf(cs: LazyList[Contract]): Contract =
    cs.fold(zero) { both(_)(_) }

  // /**
  //   */
  // def bestOf(cs: LazyList[Contract]): Contract =
  //   cs.fold(zero) { pick(_)(_) }

  // /** If `c` is worth something, take it.
  //   */
  // def optionally[F[_]: cats.Monad](c: => Contract): Contract =
  //   pick[F](c)(zero)
  //
  /**
    */
  def buy[N: Fractional](c: => Contract, amount: N, coin: InCoin): Contract =
    c combine -one(coin) * const(amount)

  /**
    */
  def sell[N: Fractional](c: => Contract, amount: N, coin: InCoin): Contract =
    -c combine one(coin) * const(amount)
}
