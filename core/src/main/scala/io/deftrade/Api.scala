package io.deftrade

/**
 Leave these here. Opaque means opaque.
  */
package opaqueid {

  import cats.{ Order }

  sealed trait IdTypeTraits extends Any {
    type KeyType
    type PhantomType
  }

  sealed trait Id[K] extends Any with IdTypeTraits {
    final type KeyType = K
    def id: K
  }

  final case class OpaqueId[K, P] private (val id: K) extends AnyVal with Id[K] {
    final type PhantomType = P
  }

  object OpaqueId {
    implicit def order[K: Order, P]: Order[OpaqueId[K, P]] = Order by (_.id)
  }

  abstract class OpaqueIdC[OIDT <: IdTypeTraits](implicit ev: cats.Order[OIDT#KeyType]) {
    ev |> discardValue
    def apply(k: OIDT#KeyType) = OpaqueId[OIDT#KeyType, OIDT#PhantomType](k)
    def fresh: OIDT            = ???
    def reserved: OIDT#KeyType = ???
  }

  object LongId {
    def reserved[P] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = OpaqueId[Int, P](Int.MinValue)
  }

}

package feralcats {

  import cats.kernel.CommutativeGroup
  import cats.kernel.instances.MapMonoid

  class MapCommutativeGroup[K, V: CommutativeGroup] extends MapMonoid[K, V] with CommutativeGroup[Map[K, V]] {
    def inverse(a: Map[K, V]): Map[K, V] =
      a.foldLeft(Map.empty[K, V]) { case (my, (k, x)) => my updated (k, CommutativeGroup inverse x) }
  }

  object instances {
    implicit def catsFeralStdCommutativeGroup[K, V: CommutativeGroup]: CommutativeGroup[Map[K, V]] =
      new MapCommutativeGroup[K, V]
  }
}

final case class Fail(msg: String) extends AnyVal
object Result {
  def apply[T](unsafe: => T): Result[T] =
    safe(unsafe).toEither.left map (x => Fail(s"${x.getClass}: ${x.getMessage}"))
}

trait Api {

  import scala.util.Try

  def safe[T](f: => T) = Try { f }

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
