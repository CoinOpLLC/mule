package io.deftrade
package model
package capital

import time._, money._

import cats.implicits._

import spire.math.Fractional

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
  type LazyList[A] = Stream[Pure, A]

  /** Random Variable representation. */
  type RV[A] = LazyList[A]

  /** placeholder */
  object RV

  /** `Value Process` representation.
    * FIXME: split this up with a specialization for date and maybe bool and fractional.
    * defeature `A`
    */
  case class PR[A] private (ozdt0: Option[ZonedDateTime], unPr: LazyList[RV[A]]) {
    def take(n: Int): PR[A]                                       = ???
    def horizon[A](pr: PR[A]): Int                                = ???
    def and(pr: PR[Boolean])(implicit ev: A =:= Boolean): Boolean = ???
  }

  /** `Value Process` primitives */
  object PR {

    private val timestep      = 1.day
    private lazy val dontcare = java.time.ZonedDateTime.now

    /** */
    def apply(zdt0: ZonedDateTime): PR[ZonedDateTime] = PR(zdt0.some, ???)

    /** */
    def apply[A](unPr: LazyList[RV[A]]): PR[A] = PR(none, unPr)

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def bigK[A](a: A): PR[A] = PR((Stream emit (Stream emit a)).repeat)

    /** */
    def date(zdt0: ZonedDateTime): PR[ZonedDateTime] = ???

    /** */
    def cond[A](yf: PR[Boolean])(then: PR[A])(elze: PR[A]): PR[A] = ???

    /**
      * A [[spire.math.Fractional]] `N` means a `Fractional PR[N]``.
      *
      * TODO: really?
      */
    implicit def fractionalPR[N: Fractional]: Fractional[PR[N]] = ???

    /** */
    def lift[A, B](f: A => B): PR[A] => PR[B] =
      pra => PR { pra.unPr map (_ map f) }

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def lift2[A, B, C](f: (A, B) => C): (PR[A], PR[B]) => PR[C] =
      (pra, prb) =>
        PR {
          pra.unPr.zipWith(prb.unPr) { (rva, rvb) =>
            rva.zipWith(rvb)(f)
          }
      }
  }

  /**
    * Observable.
    *
    * From van Straaten:
    * > An observable is thus represented as a function from a starting date to a value process. The "time-varying" nature of an observable is captured primarily by the value process itself (PR a); the Date in the function's type is simply used to specify the start date for the resulting value process.
    *
    */
  case class Obs[A](f: ZonedDateTime => PR[A])

  /** */
  object Obs {

    /** `konst(x)` is an observable that has value x at any time. */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def konst[A](a: A): Obs[A] =
      Obs(_ => PR bigK a)

    /** */
    def at(zdt: ZonedDateTime): Obs[Boolean] = ???

    // "The value of the observable date at date t is just t."
    //  date :: Obs Date
    //  date = Obs (\t -> PR $ timeSlices [t])

    // def time(zdt: ZonedDateTime): Obs[Period]      = ???
    // def wsjPrimeRate(date: LocalDate): Obs[Period] = ???
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
        when(Obs at t, one scale (Obs konst x))
    }
  }
}
