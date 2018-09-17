package io.deftrade

/**
 Leave these here. Opaque means opaque.
  */
package opaqueid {

  import cats.Eq

  sealed trait IdTypeTraits extends Any {
    type Value
    type Phantom
  }

  sealed trait Id[V] extends Any with IdTypeTraits {
    final type Value = V
    def id: V
  }

  final case class OpaqueId[V, P] private (val id: V) extends AnyVal with Id[V] {
    final type Phantom = P
  }

  object OpaqueId {
    implicit def eq[T: Eq, P]: Eq[OpaqueId[T, P]] = Eq by (_.id)
  }

  trait OpaqueIdC[OIDT <: IdTypeTraits] {
    def apply(v: OIDT#Value) = OpaqueId[OIDT#Value, OIDT#Phantom](v)
    def fresh: OIDT          = ???
    def reserved: OIDT#Value = ???
  }

  object LongId {
    def reserved[P] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = OpaqueId[Int, P](Int.MinValue)
  }

}

trait Api {

  type Or[A, B] = Either[B, A]

  /**
    * Make `Seq` immutable. See:
    * - [this post](https://hseeberger.wordpress.com/2013/10/25/attention-seq-is-not-immutable/),
    * and also
    * - [these comments](https://disqus.com/home/discussion/heikosblog/attention_seq_is_not_immutable_heikos_blog/).
    */
  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  type IndexedSeq[+A] = scala.collection.immutable.IndexedSeq[A]
  val IndexedSeq = scala.collection.immutable.IndexedSeq

  import scala.util.Try
  def safe[T, R](f: T => R): T => Try[R] = t => Try { f(t) }

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
