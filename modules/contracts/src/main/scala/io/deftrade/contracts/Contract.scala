package io.deftrade
package contracts

import spire.math.Fractional
import spire.algebra.Field

import cats.{ Eq, Eval, Group, Id, Monad, Show }
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

  /** Deferred Contract
    */
  type DefCon = Eval[Contract]

  case object Zero extends Contract

  sealed abstract case class One[F[_]] private (n: Numéraire[F]) extends Contract
  object One { def apply[F[_]](n: Numéraire[F]): One[F] = new One[F](n) {} }

  sealed abstract case class Give private (c: DefCon) extends Contract
  object Give { def apply(c: DefCon): Give = new Give(c) {} }

  sealed abstract case class Scale[N] private (
      o: Oracle[N],
      c: DefCon,
      N: Field[N]
  ) extends Contract

  object Scale {
    def apply[N: Field](o: Oracle[N], c: DefCon): Scale[N] = new Scale(o, c, Field[N]) {}
  }

  sealed abstract case class When private (o: Oracle[Boolean], c: DefCon) extends Contract
  object When { def apply(o: Oracle[Boolean], c: DefCon): When = new When(o, c) {} }

  sealed abstract case class Until private (o: Oracle[Boolean], c: DefCon) extends Contract
  object Until { def apply(o: Oracle[Boolean], c: DefCon): Until = new Until(o, c) {} }

  sealed abstract case class Anytime private (o: Oracle[Boolean], c: DefCon) extends Contract
  object Anytime { def apply(o: Oracle[Boolean], c: DefCon): Anytime = new Anytime(o, c) {} }

  sealed abstract case class Both private (cA: DefCon, cB: DefCon) extends Contract
  object Both { def apply(cA: DefCon, cB: DefCon): Both = new Both(cA, cB) {} }

  sealed abstract case class Pick[F[_]] private (cT: DefCon, cF: DefCon) extends Contract {
    def election: Oracle.Election[F]
    def choice: F[Contract]
  }
  object Pick {
    def apply[F[_]: Monad](cT: DefCon, cF: DefCon): Pick[F] =
      new Pick[F](cT, cF) {
        import cats.implicits._
        final def election: Oracle.Election[F] = ???
        final def choice: F[Contract] =
          for { b <- election.result.value } yield (if (b) cT.value else cF.value)
      }
  }

  sealed abstract case class Branch private (o: Oracle[Boolean], cT: DefCon, cF: DefCon)
      extends Contract

  object Branch {
    def apply(o: Oracle[Boolean], cT: DefCon, cF: DefCon): Branch =
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
    final def unitOf[F[_]](base: Numéraire[F]): Contract =
      One[F](base)

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
    final def branch(b: Oracle[Boolean])(cT: => Contract)(cF: => Contract): Contract =
      Branch(b, later(cT), later(cF))

    /** Party immediately receives both `cA` and `cB`.
      */
    final def both(cA: => Contract)(cB: => Contract): Contract =
      Both(later(cA), later(cB))

    /** Party immediately chooses between `cA` or `cB`.
      */
    final def pick[F[_]: Monad](cT: => Contract)(cF: => Contract): Contract =
      Pick[F](later(cT), later(cF))

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
    unitOf[Id](c)

  /** Party acquires one unit of ''something'',
    * where that ''something'' (e.g. an [[model.std.Instrument instrument]])
    * is '''non-fungable'''.
    */
  def one[F[_]](k: InKind[F]): Contract =
    unitOf[F](k)

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
