package io.deftrade
package model
package capital

import time._, money._, contracts._

/**
  * A "batteries included" library of common standardized contracts.
  */
object std {

  import Financial.Ops
  import Observable._

  /**  */
  def zeroCouponBond[N: Financial, C: Currency](
      maturity: Instant,
      face: Money[N, C]
  ): Contract =
    when(at(maturity)) { const(face.amount.to[Double]) * one }

  /** */
  def europeanCall[N: Financial, C: Currency](
      contract: => Contract,
      strike: Money[N, C],
      expiry: Instant,
  ): Contract =
    when(at(expiry)) { optionally(buy(contract, strike)) }

  /** */
  def americanCall[N: Financial, C: Currency](
      contract: => Contract,
      strike: Money[N, C],
      expiry: Instant,
  ): Contract =
    anytime(before(expiry)) { optionally(buy(contract, strike)) }
}