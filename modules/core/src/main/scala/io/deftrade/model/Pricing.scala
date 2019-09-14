package io.deftrade
package model

import money._, keyval._

import cats.implicits._

import spire.syntax.field._

import enumeratum.EnumEntry

/**
  * Price all the things.
  *
  * Something of a dumping ground for now.
  */
abstract class Pricing[MA: Financial, Q: Financial] extends Ledger[Q] {

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  final type MonetaryAmount = MA

  final type Mny[C] = Money[MonetaryAmount, C]

  /** member [[money.Financial]] instance for convenience */
  final val MonetaryAmount = Financial[MonetaryAmount]

  /** */
  sealed abstract case class TradePricer[C](mark: Trade => Result[Mny[C]])

  /**
    * FIXME: this is where the implicit pricers need to be instantiated...
    * ... um, how, exactly?
    */
  object TradePricer extends WithOpaqueKey[Long, TradePricer[_]] {

    /** */
    def apply[C: Currency: TradePricer]: TradePricer[C] = implicitly

    /** */
    def apply[C: Currency](mark: Trade => Result[Mny[C]]): TradePricer[C] =
      new TradePricer(mark) {}
  }

  /** */
  sealed abstract case class PricedTrade[C](trade: Trade, amount: Mny[C])

  /** */
  object PricedTrade {

    /**
      * TODO: Realizing now this entails implicit per-currency pricers. :|
      */
    def apply[C: Currency: TradePricer](trade: Trade): Result[PricedTrade[C]] =
      for {
        amount <- TradePricer[C] mark trade
      } yield new PricedTrade[C](trade, amount) {}

    /**
      * Reduct to "currency as `Instrument`" convention, according to the contents
      * of a certain (implicit) [[Wallet]].
      *
      * Creates what could be called a `FairTrade`...
      */
    def normalize[C: Currency](pt: PricedTrade[C])(implicit ci: Wallet[C]): Trade = ???
  }

  /** type alias */
  type ValuedFolio[C] = PricedTrade[C]

  /** */
  lazy val ValuedFolio = PricedTrade

  /**
    * Represents a price quote (in currency `C`) for instruments of type `A`.
    *
    * The two parameter type constructor takes advantage of the infix syntax; `A QuotedIn B` is
    * a human-legible expression in the domain of market quotes.
    *
    *   Instances can come from a variety of sources including live market
    *   - "Orderly market" invariant: `ask` < `bid`.
    *   - must model disorderly markets: not everything that comes at you down the wire makes sense.
    *   - used summon a pricing instance, e.g.
    *{{{val q = QuotedIn[SomeShadySpeculativeInstrument, Currency.CHF] }}}
    *       - C is intended (but not required by this base type) to represent a currency, and will
    * typically have a [[money.Currency]][C] typeclass instance
    *
    * Domain consideration: `Currency`_exchange depends on '''pricing''' of some kind.
    * One or more Market(s) determine this price.
    *     - A design that doesn't abstract over `QuotedIn`, '''including live data''', is useless.
    *     - otoh dead simple immutable for testing / demo also required
    */
  trait QuotedIn[A, C] extends Any {

    /** */
    final def bid: MonetaryAmount = quote match { case (bid, _) => bid }

    /** */
    final def ask: MonetaryAmount = quote match { case (_, ask) => ask }

    /**
      *
      * Return both a `bid` and an `ask` for the given instrument.
      *
      * Implementations which query live markets, but do not require live data,
      * should strongly consider caching.
      *
      * Domain modelling note: `quote` does not signal intent. For example, the client may ignore
      * the returned ask and just hit the bid (if selling). Servers of this api (e.g. stockbrokers)
      * cannot not predjudice their responses when asked for a quote, as the client reveals nothing
      * about their intent.
      */
    def quote: (MonetaryAmount, MonetaryAmount)

    /** */
    def tick(implicit C: Currency[C]): MonetaryAmount

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

    /**
      * Securities of any kind.
      */
    def apply[C: Currency]: QuotedIn[reference.Usin, C] = ???

    /**
      * Forex.
      */
    def apply[C1: Currency, C2: Currency]: QuotedIn[C1, C2] = ???

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
  }

  /**
    * Immutable value class representing a quote.
    *
    * TODO: reconsider the subtle name.
    */
  sealed abstract case class QuoteIn[A, C] private (
      final val quote: (MonetaryAmount, MonetaryAmount)
  ) extends QuotedIn[A, C] {

    /** */
    def tick(implicit C: Currency[C]): MonetaryAmount =
      MonetaryAmount from [BigDecimal] C.pip //  / 10 // this is a thing now
  }

  /** */
  object QuoteIn {

    /** */
    def apply[A, C2: Currency](
        bid: MonetaryAmount,
        ask: MonetaryAmount
    ): QuoteIn[A, C2] = new QuoteIn[A, C2]((bid, ask)) {}

    /** */
    def asTraded[A, C2: Currency](
        trade: MonetaryAmount
    ): QuoteIn[A, C2] = apply(trade, trade)
  }

  /** An exchange rate. */
  sealed abstract case class Rate[C1, C2]()(implicit
                                            C1: Currency[C1],
                                            C2: Currency[C2],
                                            Q: C1 QuotedIn C2) {
    import Q._

    /** */
    @inline def buy(m1: Money[MA, C1]): Money[MA, C2] = convert(m1, ask)

    /** */
    @inline def sell(m1: Money[MA, C1]): Money[MA, C2] = convert(m1, bid)

    /** */
    @inline def apply(m1: Money[MA, C1]): Money[MA, C2] = convert(m1, mid)

    /** */
    def quote: (Money[MA, C2], Money[MA, C2]) = {
      val single = C1(MonetaryAmount.one)
      (buy(single), sell(single))
    }

    /** */
    def description: String = s"""
          |Quoter buys  ${C1} and sells ${C2} at ${bid}
          |Quoter sells ${C1} and buys  ${C2} at ${ask}""".stripMargin

    /** */
    private def convert(m1: Money[MA, C1], rate: MA): Money[MA, C2] = C2(m1.amount * rate)
  }

  /** */
  object Rate {

    /** */
    def apply[C1: Currency, C2: Currency](implicit Q: C1 QuotedIn C2): Rate[C1, C2] =
      new Rate[C1, C2] {}
  }

  /** */
  implicit class SweetCurrency[C: Currency](C: C) {

    /**
      * Exchange `Rate` factory. Implicit context provides pricing.
      */
    def /[C2](cb: Currency[C2])(implicit Q: C QuotedIn C2): Rate[C, C2] = {
      implicit val C2 = cb
      Rate[C, C2]
    }
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

  sealed abstract case class TickData(tick: Tick, price: MonetaryAmount, size: Long)

  object TickData {

    /** */
    def apply(tick: Tick, price: MonetaryAmount, size: Long): TickData =
      new TickData(tick, price, size) {}

    /** */
    def bid(price: MonetaryAmount, size: Long) = TickData(Tick.Bid, price, size)

    /** */
    def ask(price: MonetaryAmount, size: Long) = TickData(Tick.Ask, price, size)

    /** */
    def trade(price: MonetaryAmount, size: Long) = TickData(Tick.Trade, price, size)
  }
}
