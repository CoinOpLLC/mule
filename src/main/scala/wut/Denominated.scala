package wut
package model

// import scala.language.implicitConversions

import java.util.{ Currency => JvmCurrency }
// import scala.util.Try

import cats.{ Eq, Monoid }
import cats.syntax.all._
import cats.{ instances => ci }
import ci.int._, ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
import ci.option._, ci.tuple._, ci.list._, ci.set._, ci.map._
import ci.eq._

// import enumeratum._
// import enumeratum.values._

//
// import eu.timepit.refined
// import refined.api.Refined
// import refined.W
// import refined.collection._
// import refined.numeric._
// import refined.auto._

// TODO: try a typeclass for currency,
// use an implicit def to turn the type bounds into a set of instances...

// Idea: `Denomination[C <: JvmCurrency]` typeclass. Construct money from their (implicit) instances.

private[model] case class Denomination(val value: JvmCurrency) /* extends AnyVal */ { self =>
  final case class Denominated[N](val amount: N) /* extends AnyVal */ {
    type AmountType = N
    def denomination: Denomination = self
    def currency: JvmCurrency      = value
  }

  import Denomination._

  implicit def eq[N: Numeric]: Eq[Denominated[N]] = Eq.fromUniversalEquals[Denominated[N]]

  implicit def monoid[N: Numeric] = new Monoid[Denominated[N]] {
    val N = implicitly[Numeric[N]]
    type DN = Denominated[N]
    override def combine(a: DN, b: DN): DN = Denominated[N](N.plus(a.amount, b.amount))
    override lazy val empty: DN            = Denominated[N](N.zero)
  }
}

object Denomination {

  private def apply(code: String): Denomination = Denomination(JvmCurrency getInstance code)

  lazy val USD = "USD" |> apply
  lazy val EUR = "EUR" |> apply

  implicit def eqN[N: Numeric]: Eq[N]         = Eq.fromUniversalEquals[N]
  implicit val eqJvmCurrency: Eq[JvmCurrency] = Eq.fromUniversalEquals[JvmCurrency]
  implicit def eq                             = Eq.fromUniversalEquals[Denomination]

}

object Denominated {
  def apply[N: Numeric](d: Denomination): N => d.Denominated[N] = n => d.Denominated(n)
}

object Examples {
  import Denomination._
  val buxxf: Int => USD.Denominated[Int] = Denominated[Int](USD)
  val buxx                               = 20 |> buxxf
  val bx2                                = 20 |> buxxf
  import USD._
  // buxx === bx2 |> assert FIXME
}
