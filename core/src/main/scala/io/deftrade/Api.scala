package io.deftrade

import cats.data.Validated

object Result {
  def apply[T](unsafe: => T): Result[T] =
    Validated catchNonFatal unsafe leftMap (x => Fail(s"${x.getClass}: ${x.getMessage}"))
  def fail[T](msg: String): Result[T] = Validated invalid Fail(msg)
}
final case class Fail(msg: String) extends AnyVal

trait Api {

  /**
    * Informs wart remover that the value is intentionally discarded.
    */
  val discardValue: Any => Unit = (_: Any) => ()

  def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

  def stripAsciiWs(s: String) = s filterNot (" \n\r\t" contains _)

  def camelToSnake(name: String): String  = CamelTo(name)("_")
  def camelToHyphen(name: String): String = CamelTo(name)("-")
  def camelToDot(name: String): String    = CamelTo(name)(".")
  def camelToWord(name: String): String   = CamelTo(name)(" ")

  object CamelTo extends CamelTo
  trait CamelTo {

    val uppers    = 'A' to 'Z'
    val nonUppers = ('a' to 'z') ++ ('0' to '9') :+ '_' :+ '$'

    def apply(sep: String)(name: String): String = {
      val osc = maybeSepFrom(sep)
      // (name |> splitCaps(osc) |> bustHumps(osc)).mkString
      val bh: Seq[Char] => Seq[Char] = bustHumps(osc)(_)
      val sc: String => Seq[Char]    = splitCaps(osc)(_)
      // val fc2: String => Seq[Char]    = fsc andThen fbh
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
