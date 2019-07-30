package io.deftrade
package money

object pricing {

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

    def ask: BigDecimal
    def bid: BigDecimal

    def tick(implicit C: Currency[C]): BigDecimal

    def isDerived: Boolean = false

    type CrossType
    def cross: Option[CrossType] = None

    @inline final def spread = ask - bid
    @inline final def mid    = bid + spread / 2
  }
  object QuotedIn {
    def apply = ???

    // FIXME: get rid of implicit case class param
    final case class Spread[A, C2](val ask: BigDecimal, val bid: BigDecimal) extends QuotedIn[A, C2] {
      def tick(implicit C2: Currency[C2]): BigDecimal = C2.pip //  / 10 // this is a thing now
    }
    final case class TradeQuote[A, C2: Currency](val trade: BigDecimal) extends /* AnyVal with */ QuotedIn[A, C2] {
      def bid: BigDecimal                             = trade
      def ask: BigDecimal                             = trade
      def tick(implicit C2: Currency[C2]): BigDecimal = C2.pip //  / 10 // this is a thing now
    }
  }

  /** An exchange rate. */
  final case class Rate[C1, C2]()(implicit
                                  C1: Currency[C1],
                                  C2: Currency[C2],
                                  Q: C1 QuotedIn C2) {
    import Q._
    @inline def buy[N: Financial](m1: Money[N, C1]): Money[N, C2]   = convert(m1, ask)
    @inline def sell[N: Financial](m1: Money[N, C1]): Money[N, C2]  = convert(m1, bid)
    @inline def apply[N: Financial](m1: Money[N, C1]): Money[N, C2] = convert(m1, mid)

    def quote[N](implicit N: Financial[N]): (Money[N, C2], Money[N, C2]) = {
      val single = C1(N.one)
      (buy(single), sell(single))
    }

    def description: String = s"""
        |Quoter buys  ${C1} and sells ${C2} at ${bid}
        |Quoter sells ${C1} and buys  ${C2} at ${ask}""".stripMargin

    private def convert[N: Financial](m1: Money[N, C1], rate: BigDecimal): Money[N, C2] = {
      val N = Financial[N]
      C2(m1.amount |> N.toBigDecimal |> (_ * rate) |> N.fromBigDecimal)
    }
  }
  implicit def inverseQuote[C1: Currency, C2: Currency](
      implicit Q: C1 QuotedIn C2
  ): C2 QuotedIn C1 =
    new QuotedIn[C2, C1] {
      def bid                             = 1 / Q.ask
      def ask                             = 1 / Q.bid
      def tick(implicit C1: Currency[C1]) = Q.tick * mid
      override def isDerived              = true
    }

  implicit def crossQuote[C1: Currency, CX: Currency, C2: Currency](
      Q1X: C1 QuotedIn CX,
      QX2: CX QuotedIn C2
  ): C1 QuotedIn C2 =
    new QuotedIn[C1, C2] {
      def bid                             = Q1X.bid * QX2.bid
      def ask                             = Q1X.ask * QX2.ask
      def tick(implicit C2: Currency[C2]) = QX2.tick(C2) * mid
      override def isDerived              = true
      type CrossType = Currency[CX]
      override def cross: Option[CrossType] = Some(Currency[CX])
    }

}
