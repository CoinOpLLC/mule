package io.deftrade

/** mixin csv read and write capabilities */
trait CsvEnum[EE <: enumeratum.EnumEntry] { self: enumeratum.Enum[EE] =>
  implicit lazy val get = CsvEnum enumGet self
  implicit lazy val put = CsvEnum.enumPut[EE]
}

/**  */
object CsvEnum {

  import enumeratum._
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
