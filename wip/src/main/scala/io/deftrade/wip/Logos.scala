package io.deftrade
package wip

import spire.math.{ Complex, Real }, Real._
import spire.syntax.order._

trait Logos {

  type Point = Complex[Real]

  case class Line(p: Point, q: Point)

  case class Triangle(a: Point, b: Point, c: Point)

  case class Circle(center: Point, radius: Real) {
    def spawn(n: Int) = for (root <- Complex rootsOfUnity n) yield copy(center = center + root)
  }

  def emanate(n: Int)(cs: Set[Circle]): Set[Circle] =
    for {
      c <- cs
      e <- c spawn n if e.center.abs > c.center.abs
    } yield e

  val seed = Set(Circle(Complex(zero, zero), one))

  def shells(n: Int)(len: Int): List[Set[Circle]] = List.iterate(seed, len)(emanate(n))

}

// scala> List.tabulate(10)(k => shells(6)(k).fold(Set.empty)(_ ++ _).size)
// res14: List[Int] = List(0, 1, 7, 19, 37, 61, 91, 127, 169, 217)

object Logos extends Logos {

  val svgTemplate =
    <svg xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" version="1.0" width="17" height="22" id="svg2"></svg>

  object Names {

    val greeks = "αβγδεζηθικλμνξοπρστυφχψω" // n.b. only one sigma

    val blackDirections: Seq[Char] = "▶▲◀▼"
    val whiteDirections: Seq[Char] = "▷△◁▽"

    val fisheye                                 = '◉' // '\u25C9'
    val bullseye                                = '◎' // '\u25CE'
    val whiteSquareContainingBlackSmallSquare   = '▣' // '\u25A3'
    val whiteUpPointingTriangleWithDot          = '◬' // '\u25EC'
    val whiteDiamondContainingBlackSmallDiamond = '◈' // '\u25C8'

    val pentagons = List('\u2b1f', '\u2b20', '\u2B53', '\u2B54', '\u2BC2')

    val whiteCircle  = '○' // '\u25CB'
    val blackCircle  = '●' // '\u25CF'
    val blackSquare  = '■' // '\u25A0'
    val whiteSquare  = '□' // '\u25A1'
    val blackDiamond = '◆' // '\u25C6'
    val whiteDiamond = '◇' // '\u25C7'

    val largeWhiteCircle = '◯' // '\u25EF
    val largeBlackCircle = '⬤' // '\u2B24
    val whiteFlag        = '⚐' // '\u2690'
    val blackFlag        = '⚑' // '\u2691'
    val blackStar        = '★' // '\u2605'
    val whiteStar        = '☆' // '\u2606'
    val blackSunWithRays = '☀' // '\u2600'
    val whiteSunWithRays = '☼' // '\u263C'

    val earth   = '♁' // '\u2641'	Antimony
    val moon    = '☽' // '\u263D'	silver
    val mercury = '☿' // '\u263F'	mercury
    val venus   = '♀' // '\u2640'	copper
    val sun     = '☉' // '\u2609'	gold
    val mars    = '♂' // '\u2642'	iron
    val jupiter = '♃' // '\u2643'	Tin
    val saturn  = '♄' // '\u2644'	Lead
    val uranus  = '♅' // '\u2645'
    val neptune = '♆' // '\u2646'
    val pluto   = '♇' // '\u2647'

    val spheres = "♁☽☿♀☉♂♃♄"

    val aries       = '♈' // '\u2648'
    val taurus      = '♉' // '\u2649'
    val gemini      = '♊' // '\u264A'
    val cancer      = '♋' // '\u264B'
    val leo         = '♌' // '\u264C'
    val virgo       = '♍' // '\u264D'
    val libra       = '♎' // '\u264E'
    val scorpius    = '♏' // '\u264F'
    val sagittarius = '♐' // '\u2650'
    val capricorn   = '♑' // '\u2651'
    val aquarius    = '♒' // '\u2652'
    val pisces      = '♓' // '\u2653'

    val zodiac = "♈♉♊♋♌♍♎♏♐♑♒♓"

    val icYang        = '⚊' // '\u268A'
    val icYin         = '⚋' // '\u268B'
    val icGreaterYang = '⚌' // '\u268C'	&#9868;
    val icLesserYin   = '⚍' // '\u268D'	&#9869;
    val icLesserYang  = '⚎' // '\u268E'	&#9870;
    val icGreaterYin  = '⚏' // '\u268F'	&#9871;
    val icHeaven      = '☰' // '\u2630'
    val icLake        = '☱' // '\u2631'
    val icFire        = '☲' // '\u2632'
    val icThunder     = '☳' // '\u2633'
    val icWind        = '☴' // '\u2634'
    val icWater       = '☵' // '\u2635'
    val icMountain    = '☶' // '\u2636'
    val icEarth       = '☷' // '\u2637'
    val icHexagrams   = ('\u4DC0' to '\u4DFF').mkString
    // ䷀䷁䷂䷃䷄䷅䷆䷇䷈䷉䷊䷋䷌䷍䷎䷏䷐䷑䷒䷓䷔䷕䷖䷗䷘䷙䷚䷛䷜䷝䷞䷟䷠䷡䷢䷣䷤䷥䷦䷧䷨䷩䷪䷫䷬䷭䷮䷯䷰䷱䷲䷳䷴䷵䷶䷷䷸䷹䷺䷻䷼䷽䷾䷿

    val lastQuarterMoon = '☾' // '\u263E'
    val atomSymbol      = '⚛' // '\u269B'	Nuclear installation
    val radioactiveSign = '☢' // '\u2622'	toxic hazard, nuclear fallout
    val biohazardSign   = '☣' // '\u2623'	disease, epidemic, pandemic
    // http://unicode.org/charts/nameslist/n_2600.html
  }
}
