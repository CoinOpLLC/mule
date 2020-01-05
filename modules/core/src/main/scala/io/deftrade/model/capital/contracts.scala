package io.deftrade
package model
package capital

import time._, money._, implicits._

import cats.implicits._
import cats.{ Order, Show }

import spire.math.Fractional
import spire.syntax.field._

/**
  * WIP that (sorta) follows the ''Composing Contracts'' work
  * by Simon Peyton Jones and Jean-Marc Eber circa 2001.
  *
  * References:
  *   - Papers at [[https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/ Microsoft Resarch]]
  *   - [[https://www.lexifi.com/apropos/ LexiFi]] sells a mature implementation of this technology in OCaml (founded by J.C. Eber, who co-authored above papers.)
  *   - Anton van Straaten's [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ Haskell implementation]]
  *       - highly referenced; no longer maintained
  *       - see in particular his section ''A note about the original papers''
  *   - Channing Walton's [[https://github.com/channingwalton/scala-contracts scala implementation]] (also not actively maintained)
  *   - [[http://netrium.org/ Netrium]]'s open source
  *   [[http://hackage.haskell.org/package/netrium Haskell implemenation]] (status?)
  *   - Financial DSLs [[http://www.dslfin.org/resources.html resource page]]
  *
  * TODO: finish off a min viable modelling set for the common instruments
  */
object contracts {

  /** Essentially arbitrary */
  lazy val tZero: Instant = java.time.Instant.EPOCH

  /** */
  type TimeSteps = Int

  /** */
  sealed abstract case class DiscreteTime(step: Int) {

    /** */
    def instant: Instant

    /** */
    def series: DiscreteTimeSeries

    /** */
    final def next: DiscreteTime = series at step + 1
  }

  /** */
  sealed abstract case class DiscreteTimeSeries private (t0: Instant, timeStep: Duration) { dts =>

    /** */
    def at(ts: TimeSteps): DiscreteTime =
      new DiscreteTime(ts) { def instant = t0 + timeStep * ts.toLong; def series = dts }
  }

  /** */
  object DiscreteTimeSeries {

    /** */
    def apply(t0: Instant): DiscreteTimeSeries = new DiscreteTimeSeries(t0, 24.hours) {}
  }

  /**
    * `random variable` representation.
    */
  type RV[A] = LazyList[A]

  /**
    * `random variable` primitives
    */
  object RV {

    /**
      * Calculates a previous slice in a lattice by averaging each adjacent pair of values
      * in the specified slice.
      */
    def prevSlice(slice: RV[Double]): RV[Double] = slice match {
      case _ if slice.isEmpty     => LazyList.empty
      case (_ #:: t) if t.isEmpty => LazyList.empty
      case (h #:: th #:: tt)      => (h + th / 2) #:: prevSlice(th #:: tt)
    }

    /**
      * TODO: in the real world, this needs params, no?
      * TODO: can make some of these tail recursive internally?
      */
    def probabilityLattice: LazyList[RV[Double]] = {

      def pathCounts: LazyList[RV[Int]] = {

        def paths(ll: LazyList[Int]): LazyList[RV[Int]] = {
          def zig = 0 #:: ll
          def zag = ll ++ LazyList(0)
          ll #:: paths(zig zip zag map { case (l, r) => l + r })
        }

        paths(LazyList(1))
      }

      def probabilities(ps: LazyList[RV[Int]]): LazyList[RV[Double]] = ps match {
        case h #:: t => (h map (_ / h.sum.toDouble)) #:: probabilities(t)
      }

      pathCounts |> probabilities
    }
  }

  /**
    * `value process` representation
    *
    * FIXME: need {{{DiscreteTime => LazyList[RV[A]]}}}
    */
  final case class PR[A] private (val unPr: LazyList[RV[A]]) extends AnyVal {
    def take(n: Int)                           = PR take (this, n)
    def horizon                                = PR horizon this
    def forall(implicit toBool: A =:= Boolean) = PR forall (toBool liftCo this)
  }

  /**
    * `value process` primitives
    *
    * Adapted from the ''How to Write a Financial Contract'' paper,
    * via van Straaten's Haskell implementation.
    */
  object PR {

    /** */
    def eval[A](o: Obs[A]): PR[A] = o match {
      case Obs.Const(a) => bigK(a)
    }

    /** */
    def eval[C: Currency](pricing: Context.Pricing): Contract => PR[Double] = {

      import Contract._

      // closure now has everything it needs: all the free vars are bound
      def eval: Contract => PR[Double] = {
        case Zero            => bigK(0.0)
        case Give(c)         => -eval(c)
        case Scale(o, c)     => (PR eval o) * eval(c)
        case And(c1, c2)     => eval(c1) + eval(c2)
        case Or(c1, c2)      => eval(c1) max eval(c2)
        case Cond(o, c1, c2) => PR.cond(PR eval o)(eval(c1))(eval(c2))
        case When(o, c)      => pricing disc [C] (PR eval o, eval(c))
        case Anytime(o, c)   => pricing snell [C] (PR eval o, eval(c))
        case Until(o, c)     => pricing absorb [C] (PR eval o, eval(c))
        case One(n) =>
          n match {
            case Numéraire.InCoin(ic) =>
              ic match {
                case Currency(c2) => pricing exch [C] c2
              }
            case Numéraire.InKind(_) => ???
          }
      }

      eval
    }

    /** */
    def bigK[A](a: A): PR[A] =
      PR(LazyList continually (LazyList continually a))

    /** */
    private def timeSlices(slice: RV[DiscreteTime]): LazyList[RV[DiscreteTime]] = {
      val (dt #:: _) = slice
      val nextStep   = dt.step + 1
      val nextSlice  = LazyList.fill(nextStep + 1)(dt.next)
      slice #:: timeSlices(nextSlice)
    }

    /**  */
    def date(t: Instant): PR[DiscreteTime] =
      PR(timeSlices(LazyList(DiscreteTimeSeries(t) at 0)))

    /** */
    def take[A](pr: PR[A], n: Int): PR[A] =
      PR(pr.unPr take n)

    /**
      * Only terminates for finite `value process`es.
      */
    def horizon[A](pr: PR[A]): Int =
      pr.unPr.size

    /**
      * idiomatic scala sematics
      *
      * Only terminates for finite value processes.
      *
      * @return true if every value in a `value process` is true, false otherwise.
      */
    def forall(pr: PR[Boolean]): Boolean = pr.unPr forall (_ forall identity)

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

    /** not doing intra-day quanting... yet... */
    val timestep = 1.day

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
    * This is true, but we'll follow the approach taken by [[http://netrium.org/ Netrium]]
    * (make `Obs` an ADT.)
    *
    * In order to align processes (`PR[A]`) which are offset in time (think calendar spreads!),
    * ''somewhere'' there has to be a function:
    * {{{
        f: Instant => PR[A]
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

    // /** FIXME implement this in io.deftrade.time please */
    // implicit def FIXME: Order[LocalDateTime] = ???

    /** `const(x)` is an observable that has value x at any time. */
    def const[A](a: A): Obs[A] = new Const(a) {}
    sealed abstract case class Const[A](a: A) extends Obs[A]

    /** */
    def at(t: Instant): Obs[Boolean] =
      const(date === const(t))

    /**
      * "The value of the observable date at date t is just t."
      *
      * TODO: define the extended ADT and DSL for `Obs` and `Contract`
      */
    def date: Obs[Instant] = const(tZero)

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

    /** Party acquires one unit of [[money.Currency]]. */
    def one[C: Currency]: Contract = new One(Currency[C]) {}

    /** Party acquires one [[Instrument]]. */
    def one(i: Instrument): Contract = new One(i) {}

    sealed abstract case class One(n: Numéraire) extends Contract

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
  }

  /**  */
  sealed trait Context

  /**  */
  object Context {

    /**
      * Lattice methods for the required stochastic process evaluation.
      *
      * Some comments adapted from the ''How to Write a Financial Contract'' paper.
      *
      *  FIXME: Many other params; will become an ADT
      */
    case class Pricing(t: Instant) extends Context {

      /**
        * Discount process.
        *
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
        * Conditions the value of a process on a [[PR]]`[Boolean]`.
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
        * Calculates the Snell envelope of `pr`, under `cond`.
        *
        * Uses the probability measure
        * associated with the type parameter [C: Currency].
        */
      def snell[C: Currency](cond: PR[Boolean], pr: PR[Double]): PR[Double] = ???

      /**
        * Returns a real-valued process representing the value of one unit of
        * the `CurrencyLike` value `k2`, expressed in `Currency` C.
        *
        * This is simply the process representing
        * the quoted exchange rate between the curencies.
        */
      def exch[C: Currency](k2: CurrencyLike): PR[Double] = ???
    }

    /**
      * The difference between "workflow automation" and "smart contracts"
      * is a matter of degree and perspective.
      *
      * FIXME: do something
      */
    case class Scheduling() extends Context
  }

  /**  */
  object standard {

    /** the name says it all */
    def muhRates(r0: Double, delta: Double): PR[Double] = ???

    import Contract._

    /**  */
    def optionally(c: Contract): Contract = c or Zero

    /**  */
    def buy[N: Financial, C: Currency](c: Contract, amount: Money[N, C]): Contract = ???

    /**  */
    def sell[N: Financial, C: Currency](c: Contract, amount: Money[N, C]): Contract = ???

    /**
      *   Extremely useful and widely referenced benchmark.
      *   - date implied by the rest of the `Contract`
      *   - time series history is necessary for backtests
      *   - parameter extraction and rate modelling is necessary for pricing
      *   - a sampling schedule should be produced by the scheduling process
      */
    def wsjPrimeRate: Obs[Double] = ???

    /**  */
    def zeroCouponBond[N: Financial, C: Currency](
        maturity: Instant,
        face: N
    ): Contract =
      when(Obs at maturity, one scale (Obs const Financial[N].to[Double](face)))

    /** */
    def europeanCall[N: Financial, C: Currency](
        contract: Contract,
        strike: Money[N, C],
        expiry: Instant,
    ): Contract =
      when(Obs at expiry, optionally(buy(contract, strike)))
  }
}
