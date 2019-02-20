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

object csvio {

  import io.chrisdavenport.cormorant._
  import io.chrisdavenport.cormorant.generic.semiauto._
  import io.chrisdavenport.cormorant.parser._
  import io.chrisdavenport.cormorant.implicits._

  import cats.implicits._

  import io.deftrade.time._
  import io.deftrade.money._

  import java.util.UUID
  // import java.time.{ Instant, LocalDate, LocalTime }

  import enumeratum._

  sealed trait Nut extends EnumEntry with Product with Serializable
  object Nut extends Enum[Nut] {
    case object Hazelnut extends Nut
    case object Peanut   extends Nut
    case object Almond   extends Nut

    lazy val values: IndexedSeq[Nut] = findValues
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
      // c: Currency[_],
      // indexAndSum: Money[Double, Currency.USD],
      ts: Instant,
  )

  import kves._
  object Foo extends WithKeyAndEq[Long, Foo] {

    def unsafeRandom = {
      val uuid = UUID.randomUUID
      val s    = uuid.toString
      val i    = s.map(_.toInt).sum
      val l    = (i * 555).toLong
      val d    = scala.math.sqrt(i.toDouble)
      val bd   = BigDecimal(d)
      val date = localDate(instant)
      val time = localTime(clockDefaultZone) // FIXME make moar orthogonal pleaz
      val nut  = Nut.Almond
      // val c    = Currency.USD
      //  val indexAndSum = Money[Double, Currency.USD]
      val ts = instant
      Foo(uuid, s, i, l, d, bd, date, time, nut, ts)
    }
  }

  // implicit def refinedGet[T: Get, P]: Get[T Refined P] = Get.tryOrMessage[T Refined P](
  //   field =>
  //     ??? // FIXME um... get the T
  //       refineV [P] (field) // passes the error along
  // )
  // implicit def refinedPut[T: Put, P]: Put[T Refined P] = stringPut.contramap(_.toString)

  final def enumeratumGet[EE <: EnumEntry](e: Enum[EE]): Get[EE] = Get.tryOrMessage(
    field => scala.util.Try(e.withName(field.x)),
    field => s"Failed to decode Enum: $e: Received Field $field"
  )
  final def enumeratumPut[EE <: EnumEntry]: Put[EE] = stringPut.contramap(_.toString)

  implicit lazy val nutGet = enumeratumGet(Nut)
  implicit lazy val nutPut = enumeratumPut[Nut]

  implicit val lr: LabelledRead[Foo] = deriveLabelledRead

  implicit val lw: LabelledWrite[Foo] = deriveLabelledWrite

  val l: List[Foo] = List.fill(3)(Foo.unsafeRandom)

  val csv = l.writeComplete.print(Printer.default)

// From String to Type
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val decoded: Either[Error, List[Foo]] = {
    parseComplete(csv)
      .leftWiden[Error]
      .flatMap(_.readLabelled[Foo].sequence)
  }
}

object fio {}
