package io.deftrade
package contracts

import syntax._, time._, money._

import cats.implicits._
import cats.{ Order, Show }
import cats.evidence._

import spire.math.Fractional
import spire.algebra.Trig
import spire.syntax.field._

/** [[eval]] lives here. */
sealed trait Engine {

  /**  What we get when we [[eval]]uate a [[Contract]]. */
  type Return

  /**  */
  def eval: Contract => Return
}

/**  */
object Engine {

  /**
    * Prices (estimates fair value of) `Contract`s.
    *
    * This implementation uses the lattice methods used in the original papers.
    *
    * TODO: refine this model
    * TODO: expand to other models
    */
  sealed abstract case class Pricing[N: Fractional, C](
      t0: Instant,
      rateModel: Pricing.PR[N]
  ) extends Engine {

    import Pricing._, Contract._, Numéraire._

    import spire.syntax.field._

    /**
      */
    final type Return = PR[N]

    /**
      * Evaluate the `Contract` in the specified [[money.Currency]], returning a real valued
      * process which represents the distribution of possible `Contract` values a given
      * number of [[Pricing.TimeSteps]] from [[t0]].
      */
    final def eval: Contract => PR[N] = {
      case Zero                   => PR.bigK(Fractional[N].one)
      case Give(c)                => -eval(c.value)
      case Scale(o: Oracle[N], c) => (Pricing eval o) * eval(c.value)
      case Both(cA, cB)           => eval(cA.value) + eval(cB.value)
      case Pick(cA, cB)           => eval(cA.value) max eval(cB.value)
      case Branch(o, cT, cF)      => PR.cond(Pricing eval o)(eval(cT.value))(eval(cF.value))
      case When(o, c)             => disc(Pricing eval o, eval(c.value))
      case Anytime(o, c)          => snell(Pricing eval o, eval(c.value))
      case Until(o, c)            => PR.absorb(Pricing eval o, eval(c.value))
      case One(n) =>
        n match {
          case InCoin(Currency(c2)) => exch(c2)
          case _: InCoin            => ???
          case _: InKind            => ???
        }
    }

    /**
      * Discounts a process `pr` under `cond`, according to the [[rateModel]].
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
    final def disc(cond: PR[Boolean], pr: PR[N]): PR[N] =
      discount(orQ, discrete)(cond, pr)

    /**
      * Calculates the Snell envelope of `pr`, under `cond` according to the [[rateModel]].
      *
      * Uses the probability measure
      * associated with the type parameter [C: Currency].
      */
    final def snell(cond: PR[Boolean], pr: PR[N]): PR[N] =
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
    final def exch(n2: Numéraire): PR[N] =
      n2 match {

        case _: Numéraire.InCoin =>
          PR bigK Fractional[N].one * 1.618

        case _: Numéraire.InKind =>
          PR bigK Fractional[N].one * 6.18
      }

    private def discount(bpq: BPQ[N], compound: Compounding[N])(
        cond: PR[Boolean],
        pr: PR[N]
    ): PR[N] = {

      def prevSlice: RV[N] => RV[N] = RV.prevSlice(Fractional[N].one * 0.5) // fair coin

      def calc(bs: LL[RV[Boolean]], ps: LL[RV[N]], rs: LL[RV[N]]): LL[RV[N]] =
        (bs, ps, rs) match {
          case (predSlice #:: bs, procSlice #:: ps, rateSlice #:: rs) =>
            if (predSlice forall identity) // TODO: true for empty slice? do we ever see?
              LL(procSlice)
            else
              calc(bs, ps, rs) match {
                case rest @ nextSlice #:: _ =>
                  def thisSlice: RV[N] = {

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
  }

  /** */
  object Pricing {

    /** This is as close as we get to Haskell's `[]`.
      */
    type LL[A] = LazyList[A]

    /**
      */
    lazy val LL = LazyList

    /**
      */
    type Compounding[N] = N => N

    /** */
    def discrete[N: Fractional]: Compounding[N] =
      r => (Fractional[N].one + r / 100.0)

    /** */
    def continuous[N: Fractional: Trig](step: TimeSteps): Compounding[N] =
      r => Trig[N].exp(r / 100.0 * step)

    /** TODO: refinements on inputs? */
    case class LatticeModelParams[N: Fractional: Trig](r: N, sigma: N, div: N, step: N) {

      val FN = Fractional[N]
      val TN = Trig[N]

      import FN.{ sqrt }
      import TN.{ exp }

      def up: N   = exp(sigma * sqrt(step))
      def down: N = 1.0 / up

      def p: N = exp((r - div) * step - down) / (up - down) // FIXME singularity no good
      def q: N = 1.0 - p
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

    /** FIXME: implement */
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

      /** not doing intra-day quanting... yet... */
      val timeStep: Duration = 24.hours // one day

      /** */
      def apply(t0: Instant): DiscreteTimeSeries = new DiscreteTimeSeries(t0, timeStep) {}
    }

    /**
      * `random variable` representation.
      */
    type RV[N] = LL[N]

    /**
      * `random variable` primitives
      */
    object RV {

      /**
        * Calculates a previous slice in a lattice by averaging each adjacent pair of values
        * in the specified slice.
        *
        * TODO: `N Refined [0,1]` would be nice here
        */
      def prevSlice[N: Fractional](p: N)(slice: RV[N]): RV[N] = slice match {
        case _ if slice.isEmpty     => LL.empty
        case (_ #:: t) if t.isEmpty => LL.empty
        case (h #:: th #:: tt)      => (h * (1 - p) + th * p) #:: prevSlice(p)(th #:: tt)
      }

      /**
        * TODO: in the real world, this needs params, no?
        * TODO: can make some of these tail recursive internally?
        */
      def probabilityLattice: LL[RV[Int]] = {

        def pathCounts: LL[RV[Int]] = {

          def paths(ll: LL[Int]): LL[RV[Int]] = {
            def zig = 0 #:: ll
            def zag = ll ++ LL(0)
            ll #:: paths(zig zip zag map { case (l, r) => l + r })
          }

          paths(LL(1))
        }

        def probabilities(ps: LL[RV[Int]]): LL[RV[Int]] = ps match {
          case h #:: t => (h map (_ / h.sum)) #:: probabilities(t)
        }

        pathCounts |> probabilities
      }

      /**
        * From the `Composing Contracts` implementation by van Straaten:
        * ''The code for absorb above does not obviously deal
        * with the expected value mentioned in the spec.
        * This is because the expected value of each
        * random variable is implicit in the value process
        * lattice representation: each node in the lattice is
        * associated with a probability, and the
        * expected value at a particular date is simply the sum
        * of the product of the value at each node
        * and its associated probability. The following functions
        * implement this calculation.''
        */
      def expectedValue[N: Fractional](outcomes: RV[N], probabilities: RV[Int]): N = {
        implicit val monoid: cats.kernel.Monoid[N] = Fractional[N].additive
        outcomes zip probabilities foldMap { case (o, p) => o * p }
      }
    }

    /**
      * `value process` representation
      */
    final case class PR[A] private (val rvs: LL[RV[A]]) extends AnyVal {
      def take(n: Int)                           = PR.take(this, n)
      def horizon                                = PR horizon this
      def forall(implicit isBool: A === Boolean) = PR forall (isBool substitute this)
    }

    /**
      * `value process` primitives
      *
      * Adapted from the ''How to Write a Fractional Contract'' paper,
      * via van Straaten's Haskell implementation.
      */
    object PR {

      private[contracts] def apply[A](rvs: LL[RV[A]]): PR[A] =
        new PR(rvs)

      /** */
      def bigK[A](a: A): PR[A] =
        PR(LL continually (LL continually a))

      /**  */
      def date(t: Instant): PR[DiscreteTime] = {

        def timeSlices(slice: RV[DiscreteTime]): LL[RV[DiscreteTime]] = {
          val (dt #:: _) = slice
          val nextStep   = dt.step + 1
          val nextSlice  = LL.fill(nextStep + 1)(dt.next)
          slice #:: timeSlices(nextSlice)
        }

        PR(timeSlices(LL(DiscreteTimeSeries(t) at 0)))
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
      def cond[A](prB: PR[Boolean])(prT: PR[A])(prF: PR[A]): PR[A] =
        lift3((b: Boolean, t: A, f: A) => if (b) t else f)(prB, prT, prF)

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
      def lift3[A, B, C, D](f: (A, B, C) => D): (PR[A], PR[B], PR[C]) => PR[D] =
        (pra, prb, prc) =>
          PR {
            pra.rvs zip (prb.rvs zip prc.rvs) map {
              case (rva, (rvb, rvc)) =>
                rva zip (rvb zip rvc) map { case (a, (b, c)) => f(a, b, c) }
            }
        }

      /** */
      def expectedValue[N: Fractional](pr: PR[N]): LL[N] =
        pr.rvs zip RV.probabilityLattice map {
          case (ps, os) => RV.expectedValue(ps, os)
        }

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
        */
      def absorb[N: Fractional](cond: PR[Boolean], pr: PR[N]): PR[N] =
        PR(
          cond.rvs zip pr.rvs map {
            case (os, ps) =>
              os zip ps map {
                case (false, p) => p
                case (true, _)  => Fractional[N].zero
              }
          }
        )

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
      * Constructs a lattice containing possible interest rates
      * given a starting rate and an increment per time step.
      *
      * FIXME: toy model; hardwired arithmetic
      */
    def rates[N: Fractional](rateNow: N, delta: N): PR[N] = {

      def makeRateSlices(rate: N, n: Int): LL[RV[N]] = {

        def rateSlice: RV[N] = LL.iterate(rate)(r => r + 2 * delta) take n

        rateSlice #:: makeRateSlices(rate - delta, n + 1)
      }

      PR(makeRateSlices(rateNow, 1))
    }

    /** */
    def eval[A](o: Oracle[A]): PR[A] =
      o match {
        case Oracle.Const(a) => PR.bigK(a)
        case _               => ???
      }

    private type BPQ[N] = (Boolean, N, N) => N
    private def orQ[N]: BPQ[N]               = (b, p, q) => if (b) p else q
    private def orMax[N: Fractional]: BPQ[N] = (b, p, q) => if (b) p else p max q
  }

  /**
    * Manual `Contract` performance workflow scheduling.
    *
    * FIXME: do something impressive
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
    * Automated `Contract` performance.
    *
    * FIXME: do something basic
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
// /**
//   *  Toy model adapted from (but not identical to) the one used in the papers.
//   */
// def toy[N: Fractional, C: Currency]: Pricing[N, C] =
//   new Pricing[N, C](
//     java.time.Instant.EPOCH,
//     Currency[C] match {
//       case Currency.CHF => rates(.0170, .00180)
//       case Currency.EUR => rates(.0165, .00025)
//       case Currency.GBP => rates(.0170, .00080)
//       case Currency.USD => rates(.0150, .00150)
//       case Currency.JPY => rates(.0110, .00250)
//       case _            => rates(.01967, .00289) // good enuf for now
//     }
//   ) {}
