package io.deftrade
package model.slices

import time._, money._, contracts._

import cats.Monad

/**
  * A "batteries included" library of common standardized contracts.
  */
object std {

  import Financial.Ops
  import Oracle._

  def cash[N: Financial, C: Currency]: Contract =
    one(Currency[C])

  /**  */
  def zeroCouponBond[N: Financial, C: Currency](
      maturity: Instant,
      face: Mny[N, C]
  ): Contract =
    when(at(maturity)) {
      one(Currency[C]) * const(face.amount)
    }

  /** */
  def europeanCall[F[_]: Monad, N: Financial, C: Currency](
      contract: => Contract,
      strike: Mny[N, C],
      expiry: Instant,
  ): Contract =
    when(at(expiry)) {
      optionally[F](buy(contract, strike.amount, Currency[C]))
    }

  /** */
  def americanCall[F[_]: Monad, N: Financial, C: Currency](
      contract: => Contract,
      strike: Mny[N, C],
      expiry: Instant,
  ): Contract =
    anytime(before(expiry)) {
      optionally[F](buy(contract, strike.amount, Currency[C]))
    }
}
