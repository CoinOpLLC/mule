package io.deftrade

import spire.math.Fractional

/**
  * Single import DSL for contract specification, representation and evaluation,
  * capturing the operational semantics of contract performance.
  */
package object contracts extends Contract.primitives {

  import Oracle._
  import Numéraire._

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
  def buy[N: Fractional](c: => Contract, amount: N, coin: InCoin): Contract =
    c combine -one(coin) * const(amount)

  /**
    */
  def sell[N: Fractional](c: => Contract, amount: N, coin: InCoin): Contract =
    -c combine one(coin) * const(amount)
}
