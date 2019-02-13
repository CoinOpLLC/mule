package io.deftrade

object util { outer =>

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
