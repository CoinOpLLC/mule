package io.deftrade
package model
package capital

import time._, money._

import fs2.{ Pure, Stream }

/**
  * WIP that follows the ''Composing Contracts'' work from Peyton Jones and Eber.
  *
  * References:
  *   - Papers at [[https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/ Microsoft Resarch]]
  *   - [[http://web.archive.org/web/20130814194431/http://contracts.scheming.org/ abandoned Haskell impl]]
  *   - [[https://github.com/channingwalton/scala-contracts Channing Walton's scala implementation]] (also not actively maintained)
  *
  * TODO: finish off a min viable modelling set for the common instruments
  */
trait contracts {

  /** Close as we get to Haskell's `[]`. */
  type LazyList[A] = Stream[Pure, A]

  /** Random Variable representation. */
  type RV[A] = LazyList[A]

  /** `Value Process` representation. */
  case class PR[A] private (unPr: LazyList[RV[A]])

  /** `Value Process` primitives */
  object PR {

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def bigK[A](a: A): PR[A] = PR { (Stream emit (Stream emit a)).repeat }

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

  /** Observable */
  case class Obs[A](f: ZonedDateTime => PR[A])

  /** */
  object Obs {

    /** `konst(x)` is an observable that has value x at any time. */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def konst[A](a: A): Obs[A] = Obs { _ =>
      PR bigK a
    }

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

    case object Zero                                                             extends Contract
    sealed abstract case class One(k: CurrencyLike)                              extends Contract
    sealed abstract case class Give(c: Contract)                                 extends Contract
    sealed abstract case class When(o: Obs[Boolean], c: Contract)                extends Contract
    sealed abstract case class Upto(o: Obs[Boolean], c: Contract)                extends Contract
    sealed abstract case class Anytime(o: Obs[Boolean], c: Contract)             extends Contract
    sealed abstract case class Scale[N: Financial](o: Obs[N], c: Contract)       extends Contract
    sealed abstract case class And(c1: Contract, c2: Contract)                   extends Contract
    sealed abstract case class Or(c1: Contract, c2: Contract)                    extends Contract
    sealed abstract case class Cond(o: Obs[Boolean], c1: Contract, c2: Contract) extends Contract

    def zero: Contract                                              = Zero
    def one[C: Currency]: Contract                                  = new One(Currency[C]) {}
    def give(c: Contract): Contract                                 = new Give(c) {}
    def when(o: Obs[Boolean], c: Contract): Contract                = new When(o, c) {}
    def upto(o: Obs[Boolean], c: Contract): Contract                = new Upto(o, c) {}
    def anytime(o: Obs[Boolean], c: Contract): Contract             = new Anytime(o, c) {}
    def scale[N: Financial](o: Obs[N], c: Contract): Contract       = new Scale(o, c) {}
    def and(c1: Contract, c2: Contract): Contract                   = new And(c1, c2) {}
    def or(c1: Contract, c2: Contract): Contract                    = new Or(c1, c2) {}
    def cond(o: Obs[Boolean], c1: Contract, c2: Contract): Contract = new Cond(o, c1, c2) {}

    /** */
    implicit class Ops(val c: Contract) {

      final def give                           = Contract give c
      final def when(o: Obs[Boolean])          = Contract when (o, c)
      final def upto(o: Obs[Boolean])          = Contract upto (o, c)
      final def anytime(o: Obs[Boolean])       = Contract anytime (o, c)
      final def scale[N: Financial](o: Obs[N]) = Contract scale (o, c)
      final def and(c2: Contract)              = Contract and (c, c2)
      final def or(c2: Contract)               = Contract or (c, c2)
    }

    /**  */
    object Common {

      /**  */
      def zeroCouponBond[N: Financial, C: Currency](t: ZonedDateTime, x: N) =
        when(Obs at t, one scale (Obs konst x))
    }
  }
}
