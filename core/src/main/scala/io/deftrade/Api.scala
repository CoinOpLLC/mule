package io.deftrade

/**
  *
  */
final class PipeToFunction1[A](val a: A) extends AnyVal {
  def |>[B](f: A => B): B = f(a)
  def p2f1[B](f: A => B): B = f(a)
}

trait Api {

  import scala.language.implicitConversions
  import scala.util.Try

  implicit def pipeToFunction1[A](a: A) = new PipeToFunction1(a)

  /**
    * Suppresses warnings from wart remover for cases were the value is intentionally discarded.
    */
  val discardValue: Any => Unit = (_: Any) => ()

  def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

  def camelToSnake(name: String): String  = camelTo(name)("_")
  def camelToHyphen(name: String): String = camelTo(name)("-")
  def camelToDot(name: String): String    = camelTo(name)(".")
  def camelToWord(name: String): String   = camelTo(name)(" ")

  /**
    * Make `Seq` immutable. See:
    * - [this post](https://hseeberger.wordpress.com/2013/10/25/attention-seq-is-not-immutable/),
    * and also
    * - [these comments](https://disqus.com/home/discussion/heikosblog/attention_seq_is_not_immutable_heikos_blog/).
    */
  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  def safe[T, R](f: T => R): T => Try[R] = t => Try { f(t) }

  object camelTo {

    val uppers    = 'A' to 'Z'
    val nonUppers = ('a' to 'z') ++ ('0' to '9') :+ '_' :+ '$'

    def apply(sep: String)(name: String): String = {
      val osc = maybeSepFrom(sep)
      (name |> splitCaps(osc) |> bustHumps(osc)).mkString
    }
    def splitCaps(sep: Option[Char])(name: String): Seq[Char] =
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

    def bustHumps(sep: Option[Char])(name: Seq[Char]): Seq[Char] =
      name.foldRight(Seq.empty[Char]) { (a, b) =>
        (a, b) match {
          case (c, h +: _) if (nonUppers contains c) && (uppers contains h) =>
            sep.fold(a +: b)(a +: _ +: b)
          case _ =>
            a +: b
        }
      }

    def maybeSepFrom(s: String): Option[Char] = s match {
      case "_" => Some('_')
      case "-" => Some('-')
      case _   => None
    }

  }

  object FansiCrap {
    import fansi.Color.{ LightMagenta }
    def colorme[S <: AnyRef](s: S): String = (fansi.Str(s.toString) overlay LightMagenta).render
    def fade(n: Int) =
      (
        (0 to 255) map { i =>
          fansi.Back.True(i, 255 - i, 255)(" ")
        } grouped n map (_.mkString)
      ) mkString "\n"
  }

}
