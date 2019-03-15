package io.deftrade

object camelsnake { outer =>

  def stripAsciiWs(s: String) = s filterNot (" \n\r\t" contains _)

  def camelToSnake(camelName: String): String  = CamelTo("_")(camelName)
  def camelToHyphen(camelName: String): String = CamelTo("-")(camelName)
  def camelToDot(camelName: String): String    = CamelTo(".")(camelName)
  def camelToWord(camelName: String): String   = CamelTo(" ")(camelName)

  implicit class CamelOps(val camelName: String) extends AnyVal {
    def camelToSnake: String  = outer.camelToSnake(camelName)
    def camelToHyphen: String = outer.camelToHyphen(camelName)
    def camelToDot: String    = outer.camelToDot(camelName)
    def camelToWord: String   = outer.camelToWord(camelName)
  }

  object CamelTo {

    val uppers    = 'A' to 'Z'
    val nonUppers = ('a' to 'z') ++ ('0' to '9') :+ '_' :+ '$'

    def apply(sep: String)(name: String): String = {
      val osc                        = maybeSepFrom(sep)
      val bh: Seq[Char] => Seq[Char] = bustHumps(osc)(_)
      val sc: String => Seq[Char]    = splitCaps(osc)(_)
      (sc andThen bh)(name).mkString
    }
    protected def splitCaps(sep: Option[Char])(name: String): Seq[Char] =
      name
        .foldLeft(Seq.empty[Char]) { (b, a) =>
          (a, b) match { // yeah just flip your head around, it's easier, trust self
            case (c, h +: g +: t)
                if (uppers contains g) &&
                  (uppers contains h) &&
                  (nonUppers contains c) => // sep between g and h
              sep.fold(c +: h +: g +: t)(c +: h +: _ +: g +: t)
            case _ => a +: b
          }
        }
        .reverse

    protected def bustHumps(sep: Option[Char])(name: Seq[Char]): Seq[Char] =
      name.foldRight(Seq.empty[Char]) { (a, b) =>
        (a, b) match {
          case (c, h +: _) if (nonUppers contains c) && (uppers contains h) =>
            sep.fold(a +: b)(a +: _ +: b)
          case _ =>
            a +: b
        }
      }

    protected def maybeSepFrom(s: String): Option[Char] = s match {
      case "_" => Some('_')
      case "-" => Some('-')
      case _   => None
    }
  }
}

import enumeratum._

/** mixin csv read and write capabilities */
trait CsvEnum[A <: EnumEntry] { self: Enum[A] =>
  implicit lazy val get = CsvEnum enumGet self
  implicit lazy val put = CsvEnum.enumPut[A]
}
object CsvEnum {

  import cats.implicits._

  import io.chrisdavenport.cormorant._
  import io.chrisdavenport.cormorant.implicits._

  /** Integrates Enumeratum into Cormorant (CSV). Use these methods to create implicits per Enum. */
  def enumGet[EE <: EnumEntry](e: Enum[EE]): Get[EE] = Get tryOrMessage (
    field => scala.util.Try { e withName field.x },
    field => s"Failed to decode Enum: $e: Received Field $field"
  )
  def enumPut[EE <: EnumEntry]: Put[EE] = stringPut contramap (_.toString)
}

object csvHacking {

  import time._
  import money._
  import kves._

  import eu.timepit.refined
  import refined.refineMV
  import refined.api.{ Refined }
  import refined.numeric.Positive

  import cats.implicits._

  import io.chrisdavenport.cormorant._
  import io.chrisdavenport.cormorant.generic.semiauto._
  import io.chrisdavenport.cormorant.parser._
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
      s: String,
      i: Int,
      l: Long,
      d: Double,
      bd: BigDecimal,
      date: LocalDate,
      time: LocalTime,
      nut: Nut,
      kBar: Bar.Key,
      x: Double Refined Positive,
      // ccy: CurrencyLike,
      // total: Money[Double, Currency.USD],
      ts: Instant,
  )

  object Foo extends WithKeyAndEq[Long, Foo] {

    /**
      * recall: our policy is to specify policy; specifically, to require that policy be specified
      */
    implicit lazy val freshKey: Fresh[Key] = Fresh.zeroBasedIncr

    def unsafeRandom: Foo = {
      val uuid = UUID.randomUUID
      val s    = uuid.toString
      val i    = s.map(_.toInt).sum
      val l    = (i * 555).toLong
      val d    = scala.math.sqrt(i.toDouble)
      val bd   = BigDecimal(d)
      val date = localDate(instant)
      val time = localTime(clockDefaultZone) // FIXME make moar orthogonal pleaz
      val nut  = Nut.Almond
      val kBar = Bar.Key.reserved
      val x    = refineMV[Positive](3.14)
      // val ccy  = Currency.USD // FIXME: why?
      //  val total = Money[Double, Currency.USD]
      val ts = instant
      Foo(
        uuid,
        s,
        i,
        l,
        d,
        bd,
        date,
        time,
        nut,
        kBar,
        x,
        // ccy,
        ts
      )
    }

    implicit lazy val readCsv: LabelledRead[Value]   = deriveLabelledRead
    implicit lazy val writeCsv: LabelledWrite[Value] = deriveLabelledWrite

    implicit lazy val readRowCsv: LabelledRead[Row]   = deriveLabelledReadRow
    implicit lazy val writeRowCsv: LabelledWrite[Row] = deriveLabelledWriteRow

    import shapeless._
    import shapeless.labelled._

    val vlg = LabelledGeneric[Value]
    type Repr    = vlg.Repr
    type RowRepr = FieldType[keyT, Key] :: Repr

  }

  LabelledRead[Foo]     |> discardValue
  LabelledRead[Foo.Row] |> discardValue

  val l: List[Foo] = List.fill(3)(Foo.unsafeRandom)

  val csv = l.writeComplete print Printer.default

  // From String to Type
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // FIXME this is probably my bug
  val decoded: Either[Error, List[Foo]] = {
    parseComplete(csv)
      .leftWiden[Error]
      .flatMap(_.readLabelled[Foo].sequence)
  }

}
