package io.deftrade

import money._, model.capital.Instrument

/**
  * Single import DSL for contract specification, representation and evaluation,
  * capturing the operational semantics of contract performance.
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
  *   Note: [[Contract]]s are not a "map of the terrain". ''They are the terrain.''
  *
  *   A `Contract` unifies all modes of performance and analysis:
  *       - captures the `operational semantics` of contract performance
  *       in a language (eDSL) that is reviewable by the parties to the `Contract`.
  *       - guides workflow plan generation for manual contract execution via [[Engine.Scheduling]]
  *       - supports smart contract execution via [[Engine.Performing]]
  *       - provides the reference for all modes of analysis
  *           - [[Engine.Pricing]] to begin
  *           - monte-carlo methods for path-dependent `Contract`s (later)
  *           - others
  */
package object contracts extends Contract.primitives {

  import Financial.Ops
  import Observable._

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
