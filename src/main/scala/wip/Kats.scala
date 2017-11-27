package wip

import cats.Eq

/**
  * Domain model class. The only domain that matters tbh.
  *
  * In a better example, these would be in their own file(s).
  */
// @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
final case class Kitteh(name: String, age: Int, color: String, favFood: String)
object Kitteh {
  def apply(name: String, age: Int, color: String): Kitteh = Kitteh(name, age, color, "Kibble")
  implicit val printable = new Printable[Kitteh] {
    override def format(k: Kitteh) = {
      import k._
      s"OH HAI $name DESU HAZ $age YAERZ AM $color EATZ $favFood K THX BYE"
    }
  }

  import cats.data.Reader
  type KittehReader[A] = Reader[Kitteh, A]
  lazy val nameReader: KittehReader[String] = Reader(_.name)

}

/** Distinguished cats! */
object Kats {
  val maru = Kitteh(name = "Maru", color = "Scottish Fold", age = 9)
  val ara  = Kitteh("Ara", 8, "Tuxedo")
}

final case class Box[A](value: A)
object Box {
  implicit def eq[A: Eq] = Eq.fromUniversalEquals[Box[A]]

  implicit def boxPrintable[A: Printable] = Printable[String].contramap { (b: Box[A]) =>
    b.toString
  }
  implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A] imap (Box[A], _.value)
}
