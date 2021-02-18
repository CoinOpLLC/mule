package io.deftrade

import money.{ Currency, Financial, Mny => Money }

/**
  * Single import DSL for contract specification, representation and evaluation,
  * capturing the operational semantics of contract performance.
  */
package object contracts extends Contract.primitives {

  import Financial.Ops
  import Oracle._
  import Numéraire.InKind

  /** Party acquires one unit of [[money.Currency]].
    */
  def one[C: Currency]: Contract =
    unitOf(Currency[C])

  /** Party acquires one unit of ''something'',
    * where that ''something'' (e.g. an [[model.Instrument instrument]])
    * is '''non-fungable'''.
    */
  def one(i: InKind): Contract =
    unitOf(i)

  /**
    */
  def allOf(cs: LazyList[Contract]): Contract =
    cs.fold(zero) { both(_)(_) }

  /**
    */
  def bestOf(cs: LazyList[Contract]): Contract =
    cs.fold(zero) { pick(_)(_) }

  /** If `c` is worth something, take it.
    */
  def optionally(c: => Contract): Contract =
    pick(c)(zero)

  /**
    */
  def buy[N: Financial, C: Currency](c: => Contract, price: Money[N, C]): Contract =
    both(c)(give { const(price.amount.to[Double]) * one })

  /**
    */
  def sell[N, C: Currency](c: Contract, price: Money[N, C])(implicit N: Financial[N]): Contract =
    both(const(price.amount.to[Double]) * one)(give { c })
}
