package io.deftrade
package test

import org.scalatest.{ prop, FlatSpec, PropSpec }, prop.GeneratorDrivenPropertyChecks

class KvesSpec extends FlatSpec {

  import io.chrisdavenport.cormorant._
  // import io.chrisdavenport.cormorant.generic.semiauto._
  import io.chrisdavenport.cormorant.parser._
  // import io.chrisdavenport.cormorant.refined._
  import io.chrisdavenport.cormorant.implicits._

  import cats.implicits._

  // import Currency.{ EUR, USD }

  "`Foo`s" should "be created randomly" in {

    import csvUnderTest._

    LabelledRead[Foo]     |> discardValue
    LabelledRead[Foo.Row] |> discardValue

    val l: List[Foo] = List.fill(3)(Foo.unsafeRandom)

    val csv = l.writeComplete print Printer.default

    // From String to Type
    val decoded: Either[Error, List[Foo]] = {
      parseComplete(csv)
        .leftWiden[Error]
        .flatMap(_.readLabelled[Foo].sequence)
    }

    decoded |> discardValue
    // val eur   = EUR
    // val eurF  = eur[Double] _
    // val eur20 = 20.0 |> eurF
    // val e20   = EUR(20.00)
    //
    // cats.kernel.Order[Money[Double, USD]]
    //
    // assert(eur20 === e20)
    //
    // assert(eur20 * 2.0 > e20)
    // assert(eur20 + eur20 > e20)
    //
    // type Dollar = Money[Double, USD] // phantom type per `Currency`
    //
    // val d20: Dollar = USD(20.00)
    // val d21: Dollar = USD(21.00)
    //
    // assert(d20 !== d21)
    // assert(d20 < d21)
    // assert((d20 max d21) === d21)
    //
    // def funge[C](den: Currency[C]): Money[Double, C] = den(19.47)
    // // def funge[C[?] <: Currency[?]](den: Moneta[C]): C[Double] = den(19.47)
    //
    // assert(USD(19.47) === funge(USD))
    //
    // val usd          = USD
    // val buck: Dollar = usd(1.0)
    // assert(buck === USD(1.0))
    // assert(buck + buck === USD(2.0))
    //
    // assert(buck.show === "USD  1.00 ")
    // assert((-buck).show === "USD (1.00)")
    //
    // import pricing._
    //
    // implicit def eurusdStaticPrice: EUR QuotedIn USD = QuotedIn.Spread(1.23, 1.22)
    //
    // lazy val eurusd = EUR / USD
    //
    // val dollarsRequired: Dollar = eurusd buy EUR(100.0)
    // val dollarsReceived: Dollar = eurusd sell EUR(100.0)
    //
    // assert(dollarsReceived < dollarsRequired)

  }
}
class KvesPropSpec extends PropSpec with GeneratorDrivenPropertyChecks {
// with TableDrivenPropertyChecks {
  property("unit is as unit does") {
    forAll { ewie: Unit =>
      assert(ewie === (()))
    }
  }
  property("doubles gonna double") {
    forAll { xs: List[Double] =>
      whenever(xs.nonEmpty) {
        import scala.language.postfixOps
        assert(math.sqrt(xs map (x => x * x) sum) >= 0)
      }
    }
  }
}

object csvUnderTest {

  import kves._
  import time._
  import money._
  import Currency.USD

  import enumeratum._

  import eu.timepit.refined
  import refined.refineMV
  import refined.api.{ Refined }
  import refined.numeric.Positive

  import cats.implicits._

  import io.chrisdavenport.cormorant._
  import io.chrisdavenport.cormorant.generic.semiauto._
  // import io.chrisdavenport.cormorant.parser._
  import io.chrisdavenport.cormorant.implicits._
  import io.chrisdavenport.cormorant.refined._

  import java.util.UUID

  /** */
  sealed trait Nut extends EnumEntry with Product with Serializable

  /** */
  object Nut extends Enum[Nut] with CsvEnum[Nut] {

    case object Peanut     extends Nut
    case object Hazelnut   extends Nut
    case object Almond     extends Nut
    case object Cashew     extends Nut
    case object Walnut     extends Nut
    case object Pecan      extends Nut
    case object Pistaschio extends Nut

    lazy val values: IndexedSeq[Nut] = findValues

  }

  case class Bar(i: Int, s: String)
  object Bar extends WithKeyAndEq[Long, Bar] {
    implicit lazy val freshKey: Fresh[Key] = Fresh.zeroBasedIncr
  }

  /** Serialize this. */
  // case class Foo[C: Currency](
  case class Foo(
      uuid: UUID,
      ts: Instant,
      s: String,
      i: Int,
      l: Long,
      d: Double,
      bd: BigDecimal,
      date: LocalDate,
      time: LocalTime,
      nut: Nut,
      bar: Bar.Key,
      x: Double Refined Positive,
      usd: Currency[USD],
      amount: Money[Double, USD],
  )

  object Foo extends WithKeyAndEq[Long, Foo] {

    def unsafeRandom: Foo = {
      val uuid   = UUID.randomUUID
      val ts     = instant
      val s      = uuid.toString
      val i      = s.map(_.toInt).sum
      val l      = (i * 555).toLong
      val d      = scala.math sqrt i.toDouble
      val bd     = d |> BigDecimal.apply
      val rn     = ts atZone zoneId
      val date   = rn.localDate
      val time   = rn.localTime
      val nut    = Nut.Almond
      val bar    = Bar.Key.reserved
      val x      = refineMV[Positive](3.14)
      val usd    = Currency.USD
      val amount = USD(1.0)
      Foo(
        uuid,
        ts,
        s,
        i,
        l,
        d,
        bd,
        date,
        time,
        nut,
        bar,
        x,
        usd,
        amount,
      )
    }

    /**
      * recall: our policy is to specify policy; specifically, to require that policy be specified
      */
    implicit lazy val freshKey: Fresh[Key] = Fresh.zeroBasedIncr

    implicit lazy val readCsv: LabelledRead[Value]   = deriveLabelledRead
    implicit lazy val writeCsv: LabelledWrite[Value] = deriveLabelledWrite

    implicit lazy val readRowCsv: LabelledRead[Row]   = deriveLabelledReadRow
    implicit lazy val writeRowCsv: LabelledWrite[Row] = deriveLabelledWriteRow

    import shapeless._

    val vlg = LabelledGeneric[Value]
    type Repr = vlg.Repr

    val rlg = LabelledGeneric[Row]
    type RowRepr = rlg.Repr // =:= labelled.FieldType[keyT, Key] :: Repr

  }
}
