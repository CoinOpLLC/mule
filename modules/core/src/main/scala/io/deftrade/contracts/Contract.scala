package io.deftrade
package contracts

import time._, money._, model.capital.Instrument

/**  This trait intentionally left blank. */
sealed trait Contract

/**  ADT definitions and constructors form the `Contract` specification DSL. */
object Contract {

  /** No rights or obligations: worthless. */
  def zero: Contract = Zero
  case object Zero extends Contract

  /** Party immediately acquires one unit of `Numéraire` (typically not invoked directly). */
  def one(n: Numéraire): Contract = new One(n) {}
  sealed abstract case class One(n: Numéraire) extends Contract

  /** Party acquires `c` multiplied by . */
  def scale(o: Obs[Double], c: Contract): Contract = new Scale(o, c) {}
  sealed abstract case class Scale(o: Obs[Double], c: Contract) extends Contract

  def give(c: Contract): Contract = new Give(c) {}
  sealed abstract case class Give(c: Contract) extends Contract

  def when(o: Obs[Boolean], c: Contract): Contract = new When(o, c) {}
  sealed abstract case class When(o: Obs[Boolean], c: Contract) extends Contract

  def until(o: Obs[Boolean], c: Contract): Contract = new Until(o, c) {}
  sealed abstract case class Until(o: Obs[Boolean], c: Contract) extends Contract

  def anytime(o: Obs[Boolean], c: Contract): Contract = new Anytime(o, c) {}
  sealed abstract case class Anytime(o: Obs[Boolean], c: Contract) extends Contract

  /** Party immediately receives both `c1` and `c2`.*/
  def and(c1: Contract, c2: Contract): Contract = new And(c1, c2) {}
  sealed abstract case class And(c1: Contract, c2: Contract) extends Contract

  /** Party immediately chooses between `c1` or `c2`. */
  def or(c1: Contract, c2: Contract): Contract = new Or(c1, c2) {}
  sealed abstract case class Or(c1: Contract, c2: Contract) extends Contract

  def branch(o: Obs[Boolean], c1: Contract, c2: Contract): Contract = new Branch(o, c1, c2) {}
  sealed abstract case class Branch(o: Obs[Boolean], cT: Contract, cF: Contract) extends Contract

  /** */
  implicit class Ops(val c: Contract) {

    final def scale(o: Obs[Double]) = Contract scale (o, c)
    final def give                  = Contract give c

    final def when(o: Obs[Boolean])    = Contract when (o, c)
    final def until(o: Obs[Boolean])   = Contract until (o, c)
    final def anytime(o: Obs[Boolean]) = Contract anytime (o, c)

    final def and(c2: Contract) = Contract and (c, c2)
    final def or(c2: Contract)  = Contract or (c, c2)
  }
}

/** Common `Contract` creation primitives. */
object standard {

  import Contract._

  /** Party acquires one unit of [[money.Currency]]. */
  def one[C: Currency]: Contract = one(Currency[C])

  /** Party acquires one unit of [[model.capital.Instrument]]. */
  def one(i: Instrument): Contract = one(i)

  /**  */
  def optionally(c: Contract): Contract = c or Zero

  /**  */
  def buy[N: Financial, C: Currency](c: Contract, price: Money[N, C]): Contract =
    c and give(one scale (Obs const Financial[N].to[Double](price.amount)))

  /**  */
  def sell[N: Financial, C: Currency](c: Contract, price: Money[N, C]): Contract =
    one scale (Obs const Financial[N].to[Double](price.amount)) and give(c)

  /**  */
  def zeroCouponBond[N: Financial, C: Currency](
      maturity: Instant,
      face: Money[N, C]
  ): Contract =
    when(Obs at maturity, one scale (Obs const Financial[N].to[Double](face.amount)))

  /** */
  def europeanCall[N: Financial, C: Currency](
      contract: Contract,
      strike: Money[N, C],
      expiry: Instant,
  ): Contract =
    when(Obs at expiry, optionally(buy(contract, strike)))

  /** */
  def americanCall[N: Financial, C: Currency](
      contract: Contract,
      strike: Money[N, C],
      expiry: Instant,
  ): Contract =
    anytime(Obs at expiry, optionally(buy(contract, strike)))
}
