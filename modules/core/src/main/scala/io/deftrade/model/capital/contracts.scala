package io.deftrade
package model
package capital

import time._, money._

import cats.implicits._

import cats.{ Order, Show }

import fs2.{ Pure, Stream }

/**
  * WIP that (sorta) follows the ''Composing Contracts'' work from Peyton Jones and Eber circa 2001.
  *
  * References:
  *   - Papers at [[https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/ Microsoft Resarch]]
  *   - Anton van Straaten's [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ Haskell implementation]] (highly referenced; no longer maintained)
  *   - Channing Walton's [[https://github.com/channingwalton/scala-contracts scala implementation]] (also not actively maintained)
  *   - [[http://hackage.haskell.org/package/netrium Netrium Haskell implemenation]] (status?)
  *   - [[https://www.lexifi.com/apropos/ LexiFi]] sells a mature implementation of this technology in OCaml.
  *   - Financial DSLs [[http://www.dslfin.org/resources.html resource page]]
  *
  * TODO: finish off a min viable modelling set for the common instruments
  */
trait contracts {

  /** Close as we get to Haskell's `[]`. */
  // type LazyList[A] = Stream[Pure, A]

  /** For ease of migration. */
  // val LazyList = Stream

  /** Random Variable representation. */
  type RV[A] = LazyList[A]

  /** */
  object RV {

    /**
      * Calculates a previous slice in a lattice by averaging each adjacent pair of values
      * in the specified slice
      */
    def prevSlice(slice: RV[Double]): RV[Double] = slice match {
      case _ if slice.isEmpty     => ???
      case (h #:: t) if t.isEmpty => ???
      case (h #:: t)              => ???
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

  /** `Value Process` primitives */
  object PR {

    /** not doing intra-day quanting... yet... */
    val timestep = 1.day

    /** */
    def apply[A](unPr: LazyList[RV[A]]): PR[A] = new PR(unPr) {}

    /** */
    def eval[A](o: Obs[A]): PR[A] = o match {
      case Obs.Const(a) => bigK(a)
    }

    /** */
    def eval[N: Financial, C: Currency](c: Context): Contract => PR[N] = c => ???

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

    /** FIXME: nice try but mentod needs to be made visible - interface? */
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

    /** */
    implicit def prOrder[A]: Order[PR[A]] = ???

    /** */
    implicit def prShow[A]: Show[PR[A]] = ???

    /**
      * A [[spire.math.Fractional]] `N` means a `Fractional PR[N].
      *
      * TODO: really?
      */
    implicit def fractionalPR[N: Fractional]: Fractional[PR[N]] = ???
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
    * Will just support constants for now, using Netrium's basic factoring but not defining
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
    def scale[N: Financial](o: Obs[N], c: Contract): Contract = new Scale(o, c) {}
    sealed abstract case class Scale[N: Financial](o: Obs[N], c: Contract) extends Contract

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

    /**
      * Party receives contract computed by `f` from value of `o` labeled by `l`.
      *
      * FIXME combine with `scale`
      */
    def sample[A](l: Label, o: Obs[A], f: A => Contract): Contract = new Sample(l, o, f) {}
    sealed abstract case class Sample[A](l: Label, o: Obs[A], f: A => Contract) extends Contract

    /** */
    implicit class Ops(val c: Contract) {

      final def scale[N: Financial](o: Obs[N]) = Contract scale (o, c)
      final def give                           = Contract give c

      final def when(o: Obs[Boolean])    = Contract when (o, c)
      final def until(o: Obs[Boolean])   = Contract until (o, c)
      final def anytime(o: Obs[Boolean]) = Contract anytime (o, c)

      final def and(c2: Contract) = Contract and (c, c2)
      final def or(c2: Contract)  = Contract or (c, c2)
    }

    /**  */
    object Common {

      /**  */
      def zeroCouponBond[N: Financial, C: Currency](t: ZonedDateTime, x: N) =
        when(Obs at t, one scale (Obs const x))
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
