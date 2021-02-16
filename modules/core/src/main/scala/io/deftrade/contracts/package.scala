package io.deftrade

import money._, model.capital.Instrument

/**
  * Single import DSL for contract specification, representation and evaluation,
  * capturing the operational semantics of contract performance.
  */
package object contracts extends Contract.primitives {

  import Financial.Ops
  import Oracle._

  /** Party acquires no rights or obligations (nothing). */
  def zero: Contract = Contract.Zero

  /** Party acquires one unit of [[money.Currency]]. */
  def one[C: Currency]: Contract = unitOf(Currency[C])

  /** Party acquires one unit of [[model.capital.Instrument]]. */
  def one(i: Instrument): Contract = unitOf(i)

  /** */
  def allOf(cs: LazyList[Contract]): Contract = cs.fold(zero) { both(_, _) }

  /** */
  def bestOf(cs: LazyList[Contract]): Contract = cs.fold(zero) { pick(_, _) }

  /** If `c` is worth something, take it. */
  def optionally(c: => Contract): Contract = pick(c, zero)

  /**  */
  def buy[N: Financial, C: Currency](c: => Contract, price: Mny[N, C]): Contract =
    both(c, give { const(price.amount.to[Double]) * one })

  /**  */
  def sell[N, C: Currency](c: Contract, price: Mny[N, C])(implicit N: Financial[N]): Contract =
    both(const(price.amount.to[Double]) * one, give { c })
}
