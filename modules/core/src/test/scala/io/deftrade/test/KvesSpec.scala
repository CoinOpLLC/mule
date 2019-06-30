package io.deftrade
package test

import cats._
import cats.implicits._
import cats.syntax.eq._

import org.scalatest.{ FlatSpec, PropSpec }

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._


object xaction {
import enumeratum._

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
  case object Brazil     extends Nut

  lazy val values: IndexedSeq[Nut] = findValues

}

  import time._, money._, kves._

  import eu.timepit.refined
  import refined.{ refineMV, refineV, W }
  import refined.api.{ Refined }
  import refined.string.MatchesRegex
  import refined.numeric.Positive
  import refined.collection.NonEmpty

  type SsnPattern = W.`"[0-9]{3}-[0-9]{2}-[0-9]{4}"`.T
  type IsSsn      = MatchesRegex[SsnPattern]

  type Ssn      = String Refined IsSsn
  type NEString = String Refined NonEmpty

  // sealed abstract
  case class Entity(ssn: Ssn, name: NEString)
  object Entity extends WithKeyAndEq[Long, Entity]

  case class Xaction(
      asOfThis: Instant,
      debitFrom: Entity.Key,
      creditTo: Entity.Key,
      amount: BigDecimal,
      memo: String
  )
  object Xaction extends WithKeyAndEq[Long, Xaction]
}

class KvesSpec extends FlatSpec {

  import io.chrisdavenport.cormorant._
  // import io.chrisdavenport.cormorant.generic.semiauto._
  import io.chrisdavenport.cormorant.parser._
  // import io.chrisdavenport.cormorant.refined._
  import io.chrisdavenport.cormorant.implicits._

  // import Currency.{ EUR, USD }

  "`Foo`s" should "be created randomly" in {

    import csvUnderTest._

    LabelledRead[Foo]     |> discardValue
    LabelledRead[Foo.Row] |> discardValue

    val x: List[Foo] = List.fill(3)(Foo.unsafeRandom)

    val csv = x.writeComplete print Printer.default

    // From String to Type
    val decoded: Either[Error, List[Foo]] = {
      parseComplete(csv)
        .leftWiden[Error]
        .flatMap(_.readLabelled[Foo].sequence)
    }

    val Some(y: List[Foo]) = decoded.toOption
    assert(x === y)
  }
}

object csvUnderTest {

  import kves._, time._, money._
  import Currency.USD

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

    /**
      * recall: our policy is to specify policy; specifically, to require that policy be specified
      */
    implicit lazy val freshKey: Fresh[Key] = Fresh.zeroBasedIncr

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
        uuid = uuid,
        ts = ts,
        s = s,
        i = i,
        l = l,
        d = d,
        bd = bd,
        date = date,
        time = time,
        nut = nut,
        bar = bar,
        x = x,
        usd = usd,
        amount = amount,
      )
    }

    import Money.{ moneyGet, moneyPut }

    implicit lazy val readCsv: LabelledRead[Value]   = deriveLabelledRead
    implicit lazy val writeCsv: LabelledWrite[Value] = deriveLabelledWrite

    implicit lazy val readRowCsv: LabelledRead[Row]   = deriveLabelledReadRow
    implicit lazy val writeRowCsv: LabelledWrite[Row] = deriveLabelledWriteRow

    // val rlg = LabelledGeneric[Row]
    // type RowRepr = rlg.Repr
    // =:= labelled.FieldType[keyT, Key] :: Repr
    // =:= KeyFieldType :: Repr

  }
}

object Jt8Gen {
  import time._
  def durationGen: Gen[Duration]                           = ???
  def finiteDurationGen(range: Duration): Gen[Duration]    = ???
  def localDateTimeInPeriod(p: Period): Gen[LocalDateTime] = ???
}

/** FIXME working on narrowing down `Arbitrary` problems */
object demoUnderTest {

  import time._
  case class Foo(i: Int, s: String, b: Boolean, nut: Nut)

  case class Bar(foo: Foo, s: String, z: Instant)

  sealed trait Base
  case class BaseIntString(i: Int, s: String)         extends Base
  case class BaseDoubleBoolean(d: Double, b: Boolean) extends Base

  implicit def arbitraryBar: Arbitrary[Bar] = ???

  // Instant, LocalDateTime, LocalDate, LocalTime, Period

  implicitly[Arbitrary[Foo]]
  implicitly[Arbitrary[Bar]]
  implicitly[Arbitrary[Base]]

}
class KvesPropSpec extends PropSpec with ScalaCheckDrivenPropertyChecks {
// with TableDrivenPropertyChecks {
  import demoUnderTest._
  property("some property about Foo") {
    forAll { foo: Foo =>
      // Ensure foo has the required property
    }
  }
}
