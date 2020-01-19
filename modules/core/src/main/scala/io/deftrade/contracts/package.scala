package io.deftrade

import time._, money._, model.capital.Instrument

/**
  * Contract specification, representation and evaluation,
  * following the ''Composing Contracts'' work
  * by Simon Peyton Jones and Jean-Marc Eber.
  *
  * A work in progress.
  *
  * References:
  *   - Papers at [[https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/ Microsoft Resarch]]
  *   - [[https://www.lexifi.com/apropos/ LexiFi]] sells a mature implementation of this technology in OCaml (founded by J.M. Eber, who co-authored above papers.)
  *   - Anton van Straaten's [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ Haskell implementation]]
  *       - highly referenced; no longer maintained
  *       - see in particular his section ''A note about the original papers''
  *   - Channing Walton's [[https://github.com/channingwalton/scala-contracts scala implementation]] (also not actively maintained)
  *   - [[http://netrium.org/ Netrium]]'s open source
  *   [[http://hackage.haskell.org/package/netrium Haskell implemenation]] (status?)
  *   - Financial DSLs [[http://www.dslfin.org/resources.html resource page]]
  *
  * FAQ:
  *   - Q: Why isn't this a subpackage of [[model]]?
  *   - A: [[Contract]]s are not a "map of the terrain". ''They are the terrain.''
  *       - execution plan for [[Engine.Scheduling]] (dumb contract execution)
  *       and [[Engine.Performing]] (smart contract execution)
  *       - reference for all modes of analysis (e.g. [[Engine.Pricing]] to begin, but
  *       eventually multi-model, including monte-carlo methods for path-dependent `Contract`s.)
  */
package object contracts extends Contract.primitives {

  /** Party acquires no rights or obligations (nothing). */
  def zero: Contract = Contract.Zero

  /** Party acquires one unit of [[money.Currency]]. */
  def one[C: Currency]: Contract = unitOf(Currency[C])

  /** Party acquires one unit of [[model.capital.Instrument]]. */
  def one(i: Instrument): Contract = unitOf(i)

  /** If `c` is worth something, take it. */
  def optionally(c: Contract): Contract = pick(c, zero)

  /**  */
  def buy[N: Financial, C: Currency](c: => Contract, price: Money[N, C]): Contract =
    both(
      c,
      give { scale(Obs const Financial[N].to[Double](price.amount)) { one } }
    )

  /**  */
  def sell[N, C: Currency](c: Contract, price: Money[N, C])(implicit N: Financial[N]): Contract =
    both(
      scale(Obs const Financial[N].to[Double](price.amount)) { one },
      give { c }
    )

  /**  */
  def zeroCouponBond[N: Financial, C: Currency](
      maturity: Instant,
      face: Money[N, C]
  ): Contract =
    when(Obs at maturity) {
      scale(Obs const Financial[N].to[Double](face.amount)) { one }
    }

  /** */
  def europeanCall[N: Financial, C: Currency](
      contract: Contract,
      strike: Money[N, C],
      expiry: Instant,
  ): Contract =
    when(Obs at expiry) {
      optionally(buy(contract, strike))
    }

  /** */
  def americanCall[N: Financial, C: Currency](
      contract: Contract,
      strike: Money[N, C],
      expiry: Instant,
  ): Contract =
    anytime(Obs before expiry) {
      optionally(buy(contract, strike))
    }
}
