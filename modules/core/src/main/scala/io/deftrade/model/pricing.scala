package io.deftrade
package model

import money._, keyval.DtEnum, implicits._

import enumeratum.EnumEntry

/**
  * Something of a dumping ground for now
  */
object pricing extends pricing

/** */
trait pricing {

  /**
    * Two parameter typeclass which takes advantage of the infix syntax: `A QuotedIn B` is
    * a human-legible expression in the domain of market quotes.
    *
    *   - can come from a variety of sources including live market
    *   - "Orderly market" invariant: `ask` < `bid`.
    *   - must model disorderly markets: not everything that comes at you down the wire makes sense.
    *   - note: the types parameters `A` and `C` are effectively *phantom types*
    *       - used summon a pricing instance
    *           - (eg `QuotedIn`[`SomeShadySpeculativeInstrument`, [[Currency.CHF]]]))
    *       - C is intended (but not required by this base type) to represent a currency, and will
    * typically have a [[Currency]][C] typeclass instance
    *
    * Domain consideration: `Currency` _exchange depends on _pricing_ of some kind.
    * One or more Market(s) determine this price.
    *     - A design that doesn't abstract over `QuotedIn`, '''including live data''', is useless.
    *     - otoh dead simple immutable for testing / demo also required
    */
  trait QuotedIn[A, C] extends Any {

    /** */
    final def ask: BigDecimal = quote match { case (_, ask) => ask }

    /** */
    final def bid: BigDecimal = quote match { case (_, bid) => bid }

    /**
      * Implementations which query live markets, but do not require live data,
      * should strongly consider caching.
      */
    def quote: (BigDecimal, BigDecimal)

    /** */
    def tick(implicit C: Currency[C]): BigDecimal

    /** */
    def isDerived: Boolean = false

    /** */
    type CrossType

    /** */
    def cross: Option[CrossType] = None

    /** */
    @inline final def spread = ask - bid

    /** */
    @inline final def mid = bid + spread / 2
  }

  /** */
  object QuotedIn {

    /** FIXME: Not sure this can be generally implemented. */
    def apply[A, C: Currency]: QuotedIn[A, C] = ???
  }

  /** Subtle name. TODO: reconsider */
  sealed abstract case class QuoteIn[A, C2] private (
      final val quote: (BigDecimal, BigDecimal)
  ) extends QuotedIn[A, C2] {

    /** */
    def tick(implicit C2: Currency[C2]): BigDecimal = C2.pip //  / 10 // this is a thing now
  }

  /** */
  object QuoteIn {

    /** */
    def apply[A, C2: Currency](
        bid: BigDecimal,
        ask: BigDecimal
    ): QuoteIn[A, C2] = new QuoteIn[A, C2]((bid, ask)) {}

    /** */
    def asTraded[A, C2: Currency](
        trade: BigDecimal
    ): QuoteIn[A, C2] = apply(trade, trade)
  }

  /** An exchange rate. */
  sealed abstract case class Rate[C1, C2]()(implicit
                                            C1: Currency[C1],
                                            C2: Currency[C2],
                                            Q: C1 QuotedIn C2) {
    import Q._

    /** */
    @inline def buy[N: Financial](m1: Money[N, C1]): Money[N, C2] = convert(m1, ask)

    /** */
    @inline def sell[N: Financial](m1: Money[N, C1]): Money[N, C2] = convert(m1, bid)

    /** */
    @inline def apply[N: Financial](m1: Money[N, C1]): Money[N, C2] = convert(m1, mid)

    /** */
    def quote[N](implicit N: Financial[N]): (Money[N, C2], Money[N, C2]) = {
      val single = C1(N.one)
      (buy(single), sell(single))
    }

    /** */
    def description: String = s"""
        |Quoter buys  ${C1} and sells ${C2} at ${bid}
        |Quoter sells ${C1} and buys  ${C2} at ${ask}""".stripMargin

    /** */
    private def convert[N: Financial](m1: Money[N, C1], rate: BigDecimal): Money[N, C2] = {
      val N = Financial[N]
      C2(m1.amount |> N.toBigDecimal |> (_ * rate) |> N.fromBigDecimal)
    }
  }

  /** */
  object Rate {

    /** */
    def apply[C1: Currency, C2: Currency](implicit Q: C1 QuotedIn C2): Rate[C1, C2] =
      new Rate[C1, C2] {}
  }

  /** */
  implicit def inverseQuote[C1: Currency, C2: Currency](
      implicit Q: C1 QuotedIn C2
  ): C2 QuotedIn C1 =
    new QuotedIn[C2, C1] {

      /** */
      def quote = (1 / Q.bid, 1 / Q.ask)

      /** */
      def tick(implicit C1: Currency[C1]) = Q.tick * mid

      /** */
      override def isDerived = true
    }

  /** */
  implicit def crossQuote[C1: Currency, CX: Currency, C2: Currency](
      Q1X: C1 QuotedIn CX,
      QX2: CX QuotedIn C2
  ): C1 QuotedIn C2 =
    new QuotedIn[C1, C2] {

      /** */
      def quote = (Q1X.bid * QX2.bid, Q1X.ask * QX2.ask)

      /** */
      def tick(implicit C2: Currency[C2]) = QX2.tick(C2) * mid

      /** */
      override def isDerived = true

      /** */
      type CrossType = Currency[CX]

      /** */
      override def cross: Option[CrossType] = Some(Currency[CX])
    }

  /** */
  sealed trait Tick extends EnumEntry with Serializable

  /** */
  object Tick extends DtEnum[Tick] {

    case object Bid   extends Tick
    case object Ask   extends Tick
    case object Trade extends Tick

    /** */
    lazy val values = findValues
  }

  sealed abstract case class TickData(tick: Tick, price: BigDecimal, size: Long)

  object TickData {

    private def mk(tick: Tick, price: BigDecimal, size: Long) = new TickData(tick, price, size) {}

    /** */
    def bid(price: BigDecimal, size: Long): TickData = mk(Tick.Bid, price, size)

    /** */
    def ask(price: BigDecimal, size: Long): TickData = mk(Tick.Ask, price, size)

    /** */
    def trade(price: BigDecimal, size: Long): TickData = mk(Tick.Trade, price, size)
  }
}
