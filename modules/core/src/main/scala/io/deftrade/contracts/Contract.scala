package io.deftrade
package contracts

/**  This trait intentionally left blank. */
sealed trait Contract

/** */
sealed abstract case class Lazy[A] private (lr: () => A) { final def value: A = lr() }

/** */
object Lazy {
  private final case class Cache[A](ua: () => A) extends (() => A) {
    lazy val cache: A  = ua()
    override def apply = cache
  }
  def apply[A](a: => A): Lazy[A] = new Lazy(Cache(() => a)) {}
}

/**
  * ADT definitions and constructors form the `Contract` specification DSL.
  *
  * FIXME: these trees are not lazy. In order to make them lazy, need to move to
  * `() => A` stored in the case class, and by-name params in the dsl methods.
  */
object Contract {

  type LzCon = Lazy[Contract]

  case object Zero                                                         extends Contract
  sealed abstract case class One(n: Numéraire)                             extends Contract
  sealed abstract case class Scale(o: Obs[Double], c: LzCon)               extends Contract
  sealed abstract case class Give(c: LzCon)                                extends Contract
  sealed abstract case class When(o: Obs[Boolean], c: LzCon)               extends Contract
  sealed abstract case class Until(o: Obs[Boolean], c: LzCon)              extends Contract
  sealed abstract case class Anytime(o: Obs[Boolean], c: LzCon)            extends Contract
  sealed abstract case class Both(c1: LzCon, c2: LzCon)                    extends Contract
  sealed abstract case class Pick(c1: LzCon, c2: LzCon)                    extends Contract
  sealed abstract case class Branch(o: Obs[Boolean], cT: LzCon, cF: LzCon) extends Contract

  /** Tucked in here so `Contract` can stay `sealed` in source file. */
  trait primitives {

    /** Party immediately acquires one unit of `Numéraire` from counterparty. */
    final def unitOf(base: Numéraire): Contract = new One(base) {}

    /** Party acquires `c` multiplied by `n`. */
    final def scale(n: Obs[Double])(c: => Contract): Contract = new Scale(n, Lazy(c)) {}

    /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`. */
    final def branch(b: Obs[Boolean])(cT: => Contract)(cF: => Contract): Contract =
      new Branch(b, Lazy(cT), Lazy(cF)) {}

    /** Party assumes role of counterparty with respect to `c`. */
    final def give(c: => Contract): Contract = new Give(Lazy(c)) {}

    /** Party will acquire c as soon as `b` is observed `true`.  */
    final def when(b: Obs[Boolean])(c: => Contract): Contract = new When(b, Lazy(c)) {}

    /** Party acquires `c` with the obligation to abandon it when `o` is observed `true`. */
    final def until(b: Obs[Boolean], c: => Contract): Contract = new Until(b, Lazy(c)) {}

    /** Once you acquire anytime obs c, you may acquire c at any time the observable obs is true. */
    final def anytime(b: Obs[Boolean])(c: => Contract): Contract = new Anytime(b, Lazy(c)) {}

    /** Party immediately receives both `c1` and `c2`. */
    final def both(c1: => Contract, c2: => Contract): Contract = new Both(Lazy(c1), Lazy(c2)) {}

    /** Party immediately chooses between `c1` or `c2`. */
    final def pick(c1: => Contract, c2: => Contract): Contract = new Pick(Lazy(c1), Lazy(c2)) {}
  }
}
