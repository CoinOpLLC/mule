package io.deftrade
package contracts

/**  This trait intentionally left blank. */
sealed trait Contract

/**
  * ADT definitions and constructors form the `Contract` specification DSL.
  *
  * FIXME: these trees are not lazy. In order to make them lazy, need to move to
  * `() => A` stored in the case class, and by-name params in the dsl methods.
  */
object Contract {

  case object Zero                                                               extends Contract
  sealed abstract case class One(n: Numéraire)                                   extends Contract
  sealed abstract case class Scale(o: Obs[Double], c: Contract)                  extends Contract
  sealed abstract case class Give(c: Contract)                                   extends Contract
  sealed abstract case class When(o: Obs[Boolean], c: Contract)                  extends Contract
  sealed abstract case class Until(o: Obs[Boolean], c: Contract)                 extends Contract
  sealed abstract case class Anytime(o: Obs[Boolean], c: Contract)               extends Contract
  sealed abstract case class And(c1: Contract, c2: Contract)                     extends Contract
  sealed abstract case class Or(c1: Contract, c2: Contract)                      extends Contract
  sealed abstract case class Branch(o: Obs[Boolean], cT: Contract, cF: Contract) extends Contract

  /** */
  implicit class Ops(val c: Contract) {

    final def give                     = new Give(c)       {}
    final def scale(n: Obs[Double])    = new Scale(n, c)   {}
    final def when(b: Obs[Boolean])    = new When(b, c)    {}
    final def until(b: Obs[Boolean])   = new Until(b, c)   {}
    final def anytime(b: Obs[Boolean]) = new Anytime(b, c) {}
    final def and(c2: Contract)        = new And(c, c2)    {}
    final def or(c2: Contract)         = new Or(c, c2)     {}
  }

  /** Tucked in here so `Contract` can stay `sealed` in source file. */
  trait primitives {

    /** Party immediately acquires one unit of `Numéraire` from counterparty. */
    def unitOf(base: Numéraire): Contract = new One(base) {}

    /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`. */
    def branch(b: Obs[Boolean], cT: Contract, cF: Contract): Contract = new Branch(b, cT, cF) {}
  }
}
