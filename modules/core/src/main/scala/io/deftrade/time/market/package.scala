package io.deftrade
package time

package object market extends market.Api {
  implicit class MarketLocalDate(val ld: LocalDate) extends AnyVal {
    def +(pp: market.ProxiedPeriod): LocalDate = ld plus pp.period
    def -(pp: market.ProxiedPeriod): LocalDate = ld minus pp.period
  }
}
