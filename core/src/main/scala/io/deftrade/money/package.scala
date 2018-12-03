package io.deftrade

package object money {
  implicit def inverseQuote[C1 <: Currency: Monetary, C2 <: Currency: Monetary](implicit Q: C1 QuotedIn C2): C2 QuotedIn C1 =
    new QuotedIn[C2, C1] {
      def bid                = 1 / Q.ask
      def ask                = 1 / Q.bid
      def tick               = Q.tick * mid
      override def isDerived = true
    }

  implicit def crossQuote[C1 <: Currency: Monetary, CX <: Currency, C2 <: Currency: Monetary](
      implicit CX: Monetary[CX],
      Q1X: C1 QuotedIn CX,
      QX2: CX QuotedIn C2
  ): C1 QuotedIn C2 =
    new QuotedIn[C1, C2] {
      def bid                = Q1X.bid * QX2.bid
      def ask                = Q1X.ask * QX2.ask
      def tick               = QX2.tick * mid
      override def isDerived = true
      type CrossType = Monetary[CX]
      override def cross: Option[CrossType] = Some(CX)
    }

}
