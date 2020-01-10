package io.deftrade
package model
package contracts

import implicits._, time._, money._

import cats.implicits._
import cats.{ Order, Show }

import spire.syntax.field._
import spire.math.Fractional

/**  */
sealed trait Engine {

  /**  */
  type Return

  /**  */
  def eval: Contract => Return
}

/**  */
object Engine {

  /**
    * Lattice methods for the required stochastic process evaluation.
    *
    * Some comments adapted from the ''How to Write a Financial Contract'' paper.
    *
    *  TODO: Many other params; may become an ADT
    */
  sealed abstract case class Pricing[C: Currency](
      t0: Instant,
      rateModels: Map[CurrencyLike, Pricing.PR[Double]]
  ) extends Engine {

    import Pricing._

    /** Constructs a lattice containing possible interest rates
      * given a starting rate and an increment per time step.
      *
      * FIXME: toy model; need plugin rateModel interface ;)
      */
    def rates(rateNow: Double, delta: Double): PR[Double] = {

      def makeRateSlices(rate: Double, n: Int): LL[RV[Double]] = {

        def rateSlice: RV[Double] = LL.iterate(rate)(r => r + 2 * delta) take n

        rateSlice #:: makeRateSlices(rate - delta, n + 1)
      }

      PR(makeRateSlices(rateNow, 1))
    }

    // /**
    //   * FIXME: toy model; need plugin rateModel interface ;)
    //   */
    // lazy val rateModels: Map[CurrencyLike, PR[Double]] =
    //   Map(
    //     Currency.CHF -> rates(1.70, 0.080),
    //     Currency.EUR -> rates(1.65, 0.025),
    //     Currency.GBP -> rates(1.70, 0.080),
    //     Currency.USD -> rates(1.50, 0.150),
    //     Currency.JPY -> rates(1.10, 0.250),
    //   )

    /** */
    def rateModel: PR[Double] =
      rateModels get Currency[C] getOrElse ???

    /**
      * FIXME: toy model; need plugin rateModel interface ;)
      */
    def discount(bpq: BPQ, compound: Compounding)(
        cond: PR[Boolean],
        pr: PR[Double]
    ): PR[Double] = {

      def prevSlice: RV[Double] => RV[Double] = RV.prevSlice(0.5)

      def calc(bs: LL[RV[Boolean]], ps: LL[RV[Double]], rs: LL[RV[Double]]): LL[RV[Double]] =
        (bs, ps, rs) match {
          case (predSlice #:: bs, procSlice #:: ps, rateSlice #:: rs) =>
            if (predSlice forall identity) // TODO: true for empty slice? do we ever see?
              LL(procSlice)
            else
              calc(bs, ps, rs) match {
                case rest @ nextSlice #:: _ =>
                  def thisSlice: RV[Double] = {

                    def discSlice =
                      prevSlice(nextSlice) zip
                        rateSlice map { case (x, r) => x * compound(r) }

                    predSlice zip
                      (procSlice zip discSlice) map { case (b, (p, d)) => bpq(b, p, d) }

                  }

                  thisSlice #:: rest
              }
        }

      PR(calc(cond.rvs, pr.rvs, rateModel.rvs))
    }

    /**
      * Discount process.
      *
      * Given a boolean-valued process `cond` , `disc`
      * transforms the real-valued process `pr`,
      * expressed in the type parameter `C: Currency`, into another real valued process.
      *
      * In states where `cond` is true, return `pr`.
      *
      * Elsewhere, the result is its "fair" equivalent stochastic value
      * process in the same `C: Currency`.
      */
    def disc(cond: PR[Boolean], pr: PR[Double]): PR[Double] =
      discount(orQ, discrete)(cond, pr)

    /**
      * Calculates the Snell envelope of `pr`, under `cond`.
      *
      * Uses the probability measure
      * associated with the type parameter [C: Currency].
      */
    def snell(cond: PR[Boolean], pr: PR[Double]): PR[Double] =
      discount(orMax, discrete)(cond, pr)

    /**
      * Returns a real-valued process representing the value of one unit of
      * the `CurrencyLike` value `c2`, expressed in `Currency` C.
      *
      * For (n: CurrencyLike), this is simply the process representing
      * the quoted exchange rate between the curencies.
      *
      * For (n: Instrument), this is simply the process representing
      * the market price of the Instrument, quoted in `Currency[C]`.
      *
      * FIXME: toy stub; implement for real.
      */
    def exch(n2: Numéraire): PR[Double] = n2 match {

      case Numéraire.InCoin(c2) =>
        c2 |> discardValue
        PR bigK 1.618

      case Numéraire.InKind(k2) =>
        k2 |> discardValue
        PR bigK 6.18
    }

    import Contract._

    final type Return = PR[Double]

    /**
      * Evaluate the `Contract` in the specified [[money.Currency]].
      * TODO: Consider moving `[C: Currency]` outside to the case class.
      */
    final def eval: Contract => PR[Double] = {
      case Zero              => PR.bigK(0.0)
      case Give(c)           => -eval(c)
      case Scale(o, c)       => (Pricing eval o) * eval(c)
      case And(c1, c2)       => eval(c1) + eval(c2)
      case Or(c1, c2)        => eval(c1) max eval(c2)
      case Branch(o, cT, cF) => PR.cond(Pricing eval o)(eval(cT))(eval(cF))
      case When(o, c)        => disc(Pricing eval o, eval(c))
      case Anytime(o, c)     => snell(Pricing eval o, eval(c))
      case Until(o, c)       => PR.absorb(Pricing eval o, eval(c))
      case One(n) => // TODO: make this a method
        n match {
          case Numéraire.InCoin(ic) =>
            ic match {
              case Currency(c2) => exch(c2)
            }
          case Numéraire.InKind(_) => ???
        }
    }
  }

  /** */
  object Pricing {

    /** */
    type LL[A] = LazyList[A]

    /** */
    lazy val LL = LazyList

    /** */
    type Compounding = Double => Double

    lazy val discrete: Compounding              = r => (1 + r / 100.0)
    def contiuous(step: TimeSteps): Compounding = r => Math.exp(r / 100.0) * step

    /** */
    type BPQ = (Boolean, Double, Double) => Double

    lazy val orQ: BPQ   = (b, p, q) => if (b) p else q
    lazy val orMax: BPQ = (b, p, q) => if (b) p else p max q

    /** Essentially arbitrary. TODO: evolve this  */
    lazy val tZero: Instant = java.time.Instant.EPOCH

    /** TODO: refinements on inputs? */
    case class LatticeModelParams(r: Double, sigma: Double, div: Double, step: Double) {

      import Math.{ exp, sqrt }

      def up: Double   = exp(sigma * sqrt(step))
      def down: Double = 1.0 / up

      def p: Double = exp((r - div) * step - down) / (up - down) // FIXME singularity no good
      def q: Double = 1.0 - p
    }

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

    /** FIXME: star me kitten */
    object DiscreteTime {

      /** */
      implicit def discreteTimeOrder: Order[DiscreteTime] = ???

      /** */
      implicit def showDiscreteTime: Show[DiscreteTime] = ???
    }

    /** */
    sealed abstract case class DiscreteTimeSeries private (
        t0: Instant,
        timeStep: Duration
    ) { dts =>

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
        *
        * TODO: `Double Refined [0,1]` would be nice here
        */
      def prevSlice(p: Double)(slice: RV[Double]): RV[Double] = slice match {
        case _ if slice.isEmpty     => LazyList.empty
        case (_ #:: t) if t.isEmpty => LazyList.empty
        case (h #:: th #:: tt)      => (h * (1 - p) + th * p) #:: prevSlice(p)(th #:: tt)
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

      /**
        * From the `Composing Contracts` implementation by van Straaten:
        * > The code for absorb above does not obviously deal
        * with the expected value mentioned in the spec.
        * This is because the expected value of each
        * random variable is implicit in the value process
        * lattice representation: each node in the lattice is
        * associated with a probability, and the
        * expected value at a particular date is simply the sum
        * of the product of the value at each node
        * and its associated probability. The following functions
        * implement this calculation.
        */
      def expectedValue(outcomes: RV[Double], probabilities: RV[Double]): Double =
        outcomes zip probabilities foldMap { case (o, p) => o * p }
    }

    /**
      * `value process` representation
      *
      * FIXME: need {{{DiscreteTime => LazyList[RV[A]]}}} somewhere!
      */
    final case class PR[A] private (val rvs: LazyList[RV[A]]) extends AnyVal {
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

      private[contracts] def apply[A](rvs: LazyList[RV[A]]): PR[A] = new PR(rvs)

      /** */
      def bigK[A](a: A): PR[A] =
        PR(LazyList continually (LazyList continually a))

      /**  */
      def date(t: Instant): PR[DiscreteTime] = {

        def timeSlices(slice: RV[DiscreteTime]): LazyList[RV[DiscreteTime]] = {
          val (dt #:: _) = slice
          val nextStep   = dt.step + 1
          val nextSlice  = LazyList.fill(nextStep + 1)(dt.next)
          slice #:: timeSlices(nextSlice)
        }

        PR(timeSlices(LazyList(DiscreteTimeSeries(t) at 0)))
      }

      /** */
      def take[A](pr: PR[A], n: Int): PR[A] =
        PR(pr.rvs take n)

      /**
        * Only terminates for finite `value process`es.
        */
      def horizon[A](pr: PR[A]): Int =
        pr.rvs.size

      /**
        * idiomatic scala sematics
        *
        * Only terminates for finite value processes.
        *
        * @return true if every value in a `value process` is true, false otherwise.
        */
      def forall(pr: PR[Boolean]): Boolean =
        pr.rvs forall (_ forall identity)

      /** */
      def cond[A](yf: PR[Boolean])(zen: PR[A])(elze: PR[A]): PR[A] = ???

      /** */
      def lift[A, B](f: A => B): PR[A] => PR[B] =
        pra => PR { pra.rvs map (_ map f) }

      /** */
      def lift2[A, B, C](f: (A, B) => C): (PR[A], PR[B]) => PR[C] =
        (pra, prb) =>
          PR {
            pra.rvs zip prb.rvs map {
              case (rva, rvb) => rva zip rvb map f.tupled
            }
        }

      /** */
      def expectedValue(pr: PR[Double]): LazyList[Double] =
        pr.rvs zip RV.probabilityLattice map { case (ps, os) => RV.expectedValue(ps, os) }

      /**
        * Conditions the value of a process on a [[PR]]`[Boolean]`.
        *
        * Given a boolean-valued process `cond`, `absorb` transforms the real-valued proess
        * `pr`, expressed with type parameter C : [[money.Currency]],
        * into another real-valued process.
        *
        * For any state, the result is the expected value of receiving p's value
        * if the region `cond` will never be true, and receiving zero in the contrary.
        *
        * In states where `cond` is true, the result is therefore zero.
        *
        * TODO: track down why `C` goes (apparently) unused
        */
      def absorb[C: Currency](cond: PR[Boolean], pr: PR[Double]): PR[Double] =
        PR(
          cond.rvs zip pr.rvs map {
            case (os, ps) =>
              os zip ps map {
                case (false, p) => p
                case (true, _)  => 0.0
              }
          }
        )

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

    /** */
    def eval[A](o: Obs[A]): PR[A] = o match {
      case Obs.Const(a) => PR.bigK(a)
    }
  }

  /**
    * `Contract` evaluation `Engine` useful for the construction of
    * manual `Contract` performance workflow scheduling.
    *
    * FIXME: do something
    */
  sealed abstract case class Scheduling() extends Engine {

    /**  */
    final type Return = cats.effect.IO[Unit]

    /**  */
    final def eval: Contract => Return = ???
  }

  /** placeholder */
  object Scheduling

  /**
    * `Contract` evaluation `Engine` predicated on the idea that the
    * difference between "workflow automation" and "smart contract execution"
    * is a matter of degree and perspective. And counterparty platform integration. (And that.)
    *
    * FIXME: implement
    */
  sealed abstract case class Performing() extends Engine {

    /**  */
    final type Return = cats.effect.IO[Unit]

    /**  */
    final def eval: Contract => Return = ???
  }

  /** placeholder */
  object Performing
}
