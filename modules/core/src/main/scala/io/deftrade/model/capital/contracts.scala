package io.deftrade
package model
package capital

import time._, money._

import cats.implicits._
import cats.{ Order, Show }

import spire.math.Fractional
import spire.syntax.field._

/**
  * WIP that (sorta) follows the ''Composing Contracts'' work from Peyton Jones and Eber circa 2001.
  *
  * References:
  *   - Papers at [[https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/ Microsoft Resarch]]
  *   - [[https://www.lexifi.com/apropos/ LexiFi]] sells a mature implementation of this technology in OCaml (founded by J.C. Eber, who co-authored above papers.)
  *   - Anton van Straaten's [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ Haskell implementation]] (highly referenced; no longer maintained)
  *   - Channing Walton's [[https://github.com/channingwalton/scala-contracts scala implementation]] (also not actively maintained)
  *   - [[http://hackage.haskell.org/package/netrium Netrium Haskell implemenation]] (status?)
  *   - Financial DSLs [[http://www.dslfin.org/resources.html resource page]]
  *
  * TODO: finish off a min viable modelling set for the common instruments
  */
trait contracts {

  /**
    * Random Variable representation.
    *
    * Close as we get to Haskell's `[]`.
    */
  type RV[A] = LazyList[A]

  /** */
  object RV {

    /**
      * Calculates a previous slice in a lattice by averaging each adjacent pair of values
      * in the specified slice
      */
    def prevSlice(slice: RV[Double]): RV[Double] = slice match {
      case _ if slice.isEmpty     => ???
      case (_ #:: t) if t.isEmpty => ???
      case (_ #:: _)              => ???
    }

    /** the name says it all */
    def muhRates(r0: Double, delta: Double): PR[Double] = ???

    /** TODO: in the real world, this needs params, no? */
    def probabilityLattice: LazyList[RV[Double]] = ???
  }

  /** `Value Process` representation.
    * FIXME: split this up with a specialization for date and maybe bool and fractional.
    * defeature `A`
    */
  sealed abstract case class PR[A] private (unPr: LazyList[RV[A]])

  /**
  `Value Process` primitives.
    *
    * Adapted from the ''How to Write a Financial Contract'' paper

    */
  object PR {

    /** not doing intra-day quanting... yet... */
    val timestep = 1.day

    /** */
    def apply[A](unPr: LazyList[RV[A]]): PR[A] = new PR(unPr) {}

    /** */
    def eval[A](o: Obs[A]): PR[A] = o match {
      case Obs.Const(a) => bigK(a)
    }

    import Contract._

    /** */
    def eval[C: Currency](ctx: Context)(c: Contract): Contract => PR[Double] = {

      // import ctx._

      // closure now has everything it needs: all the free vars are bound
      def eval: Contract => PR[Double] = {
        case Zero            => bigK(0.0)
        case One(c2)         => exch[C](c2)
        case Give(c)         => -eval(c)
        case Scale(o, c)     => (PR eval o) * eval(c)
        case And(c1, c2)     => eval(c1) + eval(c2)
        case Or(c1, c2)      => eval(c1) max eval(c2)
        case Cond(o, c1, c2) => cond(PR eval o)(eval(c1))(eval(c2))
        case When(o, c)      => disc[C](PR eval o, eval(c))
        case Anytime(o, c)   => snell[C](PR eval o, eval(c))
        case Until(o, c)     => absorb[C](PR eval o, eval(c))
      }
      eval
    }

    /** */
    def take[A](n: Int): PR[A] => PR[A] = ???

    /** */
    def horizon[A]: PR[A] => Int = ???

    /** */
    def and: PR[Boolean] => Boolean = ???

    /** */
    def bigK[A](a: A): PR[A] = PR(LazyList continually (LazyList continually a))

    /** */
    def slicesFrom(zdr: ZonedDateTime): LazyList[RV[ZonedDateTime]] = ???

    /**  */
    def date(zdt0: ZonedDateTime): PR[ZonedDateTime] =
      PR(slicesFrom(zdt0))

    /** */
    def cond[A](yf: PR[Boolean])(zen: PR[A])(elze: PR[A]): PR[A] = ???

    /** */
    def lift[A, B](f: A => B): PR[A] => PR[B] =
      pra => PR { pra.unPr map (_ map f) }

    /** */
    def lift2[A, B, C](f: (A, B) => C): (PR[A], PR[B]) => PR[C] =
      (pra, prb) =>
        PR {
          pra.unPr zip prb.unPr map {
            case (rva, rvb) => rva zip rvb map f.tupled
          }
      }

    /**
      * Given a boolean-valued process `cond` , `disc`
      * transforms the real-valued process `pr`,
      * expressed in the type parameter `C: Currency`, into another real valued process.
      * In states where `cond` is true, return `pr`.
      *
      * Elsewhere, the result is its "fair" equivalent stochastic value
      * process in the same `C: Currency`.
      */
    def disc[C: Currency](cond: PR[Boolean], pr: PR[Double]): PR[Double] = ???

    /**
      * Adapted from the ''How to Write a Financial Contract'' paper:
      *
      * Given a boolean-valued proess `cond`, `absorb` transforms the real-valued proess
      * `pr`, expressed with type parameter C : [[money.Currency]],
      * into another real-valued process.
      *
      * For any state, the result is the expected value of receiving p's value
      * if the region `cond` will never be true, and receiving zero in the contrary.
      *
      * In states where `cond` is true, the result is therefore zero.
      */
    def absorb[C: Currency](cond: PR[Boolean], pr: PR[Double]): PR[Double] = ???

    /**
      * Calculates the Snell envelope of `pr` , under `cond`.
      * It uses the probability measure
      * associated with the type parameter [C: Currency].
      */
    def snell[C: Currency](cond: PR[Boolean], pr: PR[Double]): PR[Double] = ???

    /**
      *
      * Returns a real-valued process representing the value of one unit of
      * the `CurrencyLike` value `k2`, expressed in `Currency` C.
      *
      * This is simply the process representing
      * the quoted exchange rate between the curencies.
      */
    def exch[C: Currency](k2: CurrencyLike): PR[Double] = ???

    /** */
    implicit def prOrder[A]: Order[PR[A]] = ???

    /** */
    implicit def prShow[A]: Show[PR[A]] = ???

    /**
      * A [[spire.math.Fractional]] `N` means a `Fractional PR[N].
      *
      * FIXME: implement!
      */
    implicit def prFractional[N: Fractional]: Fractional[PR[N]] = ???
  }

  /**
    * From van Straaten:
    * > An `Obs`ervable is thus represented as a function from a starting date to a value process. The "time-varying" nature of an observable is captured primarily by the value process itself (PR a); the Date in the function's type is simply used to specify the start date for the resulting value process.
    *
    * This is true, but we'll follow the approach of [[http://netrium.org/ Netrium]]
    * (make `Obs` an ADT.)
    *
    * In order to align processes (`PR[A]`) which are offset in time (think calendar spreads!),
    * ''somewhere'' there has to be a function:
    * {{{
        f: ZonedDateTime => PR[A]
      }}}
    *
    * No examples can be located where processes offset in time are supported;
    * the released Netrium package has the ability to store a single date
    * in the `Model` (and doesn't use that).
    *
    * Will just support constants for now, implementing `Obs` as an ADT but not defining
    * or implementing any Contract execution capabilities, which could change because
    *   - a move to Monadic Contract definition
    *   - distributed ledger enabling (see e.g. `Fae`)
    */
  sealed trait Obs[A]

  /**
    *
    */
  object Obs {

    /** FIXME implement this in io.deftrade.time please */
    implicit def FIXME: Order[ZonedDateTime] = ???

    /** `const(x)` is an observable that has value x at any time. */
    def const[A](a: A): Obs[A] = new Const(a) {}
    sealed abstract case class Const[A](a: A) extends Obs[A]

    /** */
    def at(zdt: ZonedDateTime): Obs[Boolean] =
      const(date === const(zdt))

    /**
      * "The value of the observable date at date t is just t."
      * `date :: Obs Date`
      * `date = Obs (\t -> PR $ timeSlices [t])`
      */
    def date: Obs[ZonedDateTime] = ???

    /** FIXME: is this how we want the interface to look? */
    def wsjPrimeRate(date: LocalDate): Obs[Double] = ???

    /** */
    implicit def obsOrder[A: Order]: Order[Obs[A]] = ???

    /** */
    implicit def obsShow[A]: Show[Obs[A]] = ???
  }

  /** */
  sealed trait Contract

  /** */
  object Contract {

    /** No rights or obligations. */
    def zero: Contract = Zero
    case object Zero extends Contract

    /** Party acquires one unit of Currency. FIXME make general for stawks. */
    def one[C: Currency]: Contract = new One(Currency[C]) {}
    sealed abstract case class One(k: CurrencyLike) extends Contract

    /** Party acquires `c` multiplied by . */
    def scale(o: Obs[Double], c: Contract): Contract = new Scale(o, c) {}
    // def scale[N: Financial](o: Obs[N], c: Contract): Contract = new Scale(o, c) {}
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

    def cond(o: Obs[Boolean], c1: Contract, c2: Contract): Contract = new Cond(o, c1, c2) {}
    sealed abstract case class Cond(o: Obs[Boolean], c1: Contract, c2: Contract) extends Contract

    // /**
    //   * Party receives contract computed by `f` from value of `o` labeled by `l`.
    //   *
    //   * FIXME combine with `scale`
    //   */
    // def sample[A](l: Label, o: Obs[A], f: A => Contract): Contract = new Sample(l, o, f) {}
    // sealed abstract case class Sample[A](l: Label, o: Obs[A], f: A => Contract) extends Contract

    /** */
    implicit class Ops(val c: Contract) {

      // final def scale[N: Financial](o: Obs[N]) = Contract scale (o, c)
      final def scale(o: Obs[Double]) = Contract scale (o, c)
      final def give                  = Contract give c

      final def when(o: Obs[Boolean])    = Contract when (o, c)
      final def until(o: Obs[Boolean])   = Contract until (o, c)
      final def anytime(o: Obs[Boolean]) = Contract anytime (o, c)

      final def and(c2: Contract) = Contract and (c, c2)
      final def or(c2: Contract)  = Contract or (c, c2)
    }

    /**  */
    def optionally(c: Contract): Contract = c or Zero

    /**  */
    def buy[N: Financial, C: Currency](c: Contract, amount: Money[N, C]): Contract = ???

    /**  */
    def sell[N: Financial, C: Currency](c: Contract, amount: Money[N, C]): Contract = ???

    /**  */
    object Common {

      /**  */
      def zeroCouponBond[N: Financial, C: Currency](maturity: ZonedDateTime, face: N) =
        when(Obs at maturity, one scale (Obs const Financial[N].to[Double](face)))

      /** */
      def europeanCall[N: Financial, C: Currency](
          contract: Contract,
          strike: Money[N, C],
          expiry: ZonedDateTime,
      ): Contract =
        when(Obs at expiry, optionally(buy(contract, strike)))
    }
  }

  /**  */
  sealed trait Context

  /**  */
  object Context {

    /**  FIXME: Many other params */
    case class Pricing(zdt: ZonedDateTime) extends Context

    /**  TODO: something */
    case class Execution() extends Context
  }
}
