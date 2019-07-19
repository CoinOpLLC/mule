package io.deftrade
package test

import scala.language.higherKinds

import time._, money._, keyval._, model._, repos._, capital.Instrument, Currency.USD

import enumeratum._

import cats._
import cats.implicits._
import cats.syntax.eq._ // ups the implicit priority (?!)

import eu.timepit.refined
import refined.{ refineMV }
import refined.api.Refined
import refined.collection.NonEmpty
import refined.numeric.Positive

import io.chrisdavenport.cormorant
import cormorant._
import cormorant.generic.auto._
import cormorant.parser._
import cormorant.refined._
import cormorant.implicits._

import org.scalatest.{ FlatSpec, PropSpec }

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._

/** Nuts exist in the test package. Make of that what you will. */
sealed trait Nut extends EnumEntry with Serializable

/** All the nuts I could think of. */
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

object minviablethingie {

  final case class Foo(d: Double, s: String Refined NonEmpty, b: Boolean)

  object Foo extends WithOpaqueKey[Long, Foo] {
    def mk(s: String Refined NonEmpty): Foo =
      Foo(
        s = s,
        d = s.value.length / 17.0,
        b = s.value.isEmpty || ((s.value.head.toInt % 2) == 0)
      )
  }

  final case class Bar(fk: Foo.Key)
  object Bar extends WithOpaqueKey[Long, Bar]

}

object xaction {

  type SsnPattern = refined.W.`"[0-9]{3}-[0-9]{2}-[0-9]{4}"`.T
  type IsSsn      = refined.string.MatchesRegex[SsnPattern]
  type Ssn        = String Refined IsSsn

  type NonEmptyString = String Refined NonEmpty

  sealed abstract case class Entity private (ssn: Ssn, name: NonEmptyString)

  object Entity extends WithOpaqueKey[Long, Entity] {
    def apply(ssn: Ssn, name: NonEmptyString): Entity = new Entity(ssn, name) {}
  }

  sealed abstract case class Xaction[N: Financial, C: Currency](
      asOf: Instant,
      debitFrom: Entity.Key,
      creditTo: Entity.Key,
      amount: Money[N, C],
      memo: String
  )

  object Xaction extends WithOpaqueKey[Long, Xaction[BigDecimal, Currency.USD]]
}

class KvesSpec extends FlatSpec {

  "`Foo`s" should "be created randomly" in {

    import refined.auto._

    // import csvUnderTest._
    import minviablethingie._

    implicit lazy val freshFooKey: Fresh[Foo.Key] = Fresh.zeroBasedIncr

    val xs: List[Foo]       = List.fill(3)(Foo mk "yo wtf")
    val ks: List[Foo.Key]   = xs map (_ => Fresh[Foo.Key].init)
    val rows: List[Foo.Row] = ks zip xs

    val csv = rows.writeComplete print Printer.default

    // From String to Type
    val decoded: Either[Error, List[Foo.Row]] = {
      parseComplete(csv)
        .leftWiden[Error]
        .flatMap(_.readLabelled[Foo.Row].sequence)
    }

    val Some(roundTripRows: List[Foo.Row]) = decoded.toOption
    assert(rows === roundTripRows)
  }
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

object csvUnderTest {

  import keyval._
  import time._
  import money._
  import Currency.USD

  import java.util.UUID

  case class Bar(i: Int, s: String)
  object Bar extends WithOpaqueKey[Long, Bar] {
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

  object Foo extends WithOpaqueKey[Long, Foo] {

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
  }
}

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

object Jt8Gen {
  import time._
  def durationGen: Gen[Duration]                           = ???
  def finiteDurationGen(range: Duration): Gen[Duration]    = ???
  def localDateTimeInPeriod(p: Period): Gen[LocalDateTime] = ???
}

object IsinScratch {
  val isins = List(
    "US0378331005",
    "US0373831005",
    "U50378331005",
    "US03378331005",
    "AU0000XVGZA3",
    "AU0000VXGZA3",
    "FR0000988040"
  )

}

/** */
object Repos extends WithOpaqueKey[Long, OMS[Id]] {

  /** */
  type LegalEntities = LegalEntity.Table

  /** */
  implicit def w00t: Fresh[LegalEntity.Key] = ??? // to compile duh
  /** */
  object LegalEntities extends SimplePointInTimeRepository[_root_.cats.Id, LegalEntity.Key, LegalEntity]

  /**
    * `CashInstruments`:
    * - is a configuration parameter only.
    * - is not as a repository, or store.
    * - shall never have F[_] threaded through it.
    *
    * All `Currency` instances in scope are required to have a `CashInstruments` instance.
    */
  object CashInstruments {

    def apply[C: Currency]: Wallet[C] = (cash get Currency[C]).fold(???) { x =>
      Wallet apply [C] Folios(x)
    }

    private lazy val cash: Map[CurrencyLike, Folio.Key] = Map.empty
  }

  /** */
  type CashInstruments = Wallet.Table

  /** */
  object Instruments extends MemInsertableRepository[_root_.cats.Id, Instrument.Key, Instrument]

  /** */
  type Instruments = Instrument.Table

  implicit def eq: Eq[Folio] = ???

  /** */
  object Folios extends SimplePointInTimeRepository[_root_.cats.Id, Folio.Key, Folio] {
    def apply(id: Folio.Key): Folio = get(id).fold(Folio.empty)(identity)
  }

  /** */
  type Folios = Folio.Table

  /** */
  implicit def freshAccountNo: Fresh[Account.Key] = ??? // to compile duh

  /** */
  object Accounts extends SimplePointInTimeRepository[_root_.cats.Id, Account.Key, Account]

  /** */
  type Accounts = Account.Table

  /** FIXME: this is just a placeholder - needs to reference [[Transaction]] */
  type Transactions[F[_]] = Foldable[F]

  /** FIME this becomes a stream like repo (???)      */
  object Transactions {}

  /** */
  lazy val Markets: Repository[_root_.cats.Id, Market.Key, Market] =
    SimplePointInTimeRepository[_root_.cats.Id, Market.Key, Market]()

  /** */
  type Markets = Markets.Table

  /**
    *
    *  this is something of an abuse of the original PiT concept,
    * which models slowly evolving entities *with identity (key) which survives updates.
    *
    *  `Orders` is exactly the opposite.
    *
    *  But the open date range for "current `Table`" models the "open orders" concept perfectly.
    *
    *  TODO: is this really worthwhile?
    *
    */
  type Orders = model.Order.Table

  /** */
  object Orders extends SimplePointInTimeRepository[_root_.cats.Id, model.Order.Key, model.Order[USD]]

  /**  n.b. `Exectutions` are recorded as [[Transactions]] this completing the life cycle */
  type Executions = Executions.Table

  /** */
  object Executions extends MemAppendableRepository[_root_.cats.Id, Execution.Key, Execution]

}
// abstract class Playground[MA: Financial, Q: Financial] extends Fruitcake[MA, Q] { api =>
// }
