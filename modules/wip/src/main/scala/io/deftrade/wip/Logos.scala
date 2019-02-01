package io.deftrade
package wip

import java.nio.file.{ Files, Paths }

import spire.math.{ Complex, Real }, Real._
import spire.syntax.order._

trait Logos {

  trait EmanationLike {

    protected def m: Int
    protected def n: Int

    protected def al: Char
    protected def om: Char

    final lazy val alpha: Int => Char = i => alphas(i % m) // TODO: s is unsigned; i is not.
    final lazy val omega: Int => Char = i => omegas(i % n)

    private lazy val alphas: Seq[Char] = al until (al + m).toChar
    private lazy val omegas: Seq[Char] = om until (om + n).toChar
  }

  case class Emanation(m: Int, n: Int, al: Char, om: Char) extends EmanationLike

  /**
    * m: number of shells
    * n: coeficient of theta (omega := i theta; theta := nth root of unity)
    */
  def emanation(m: Int, n: Int) = Emanation(m, n, 'a', '0')

  type Point = Complex[Real]

  def point(p: Point): String  = s"${real(p)},${imag(p)}"
  def real(p: Point): String   = s"${p.real}"
  def imag(p: Point): String              = s"${p.imag}"

  sealed trait Graphic { self =>
    final val TissueThin = 0.05
    def svg = self match { // TODO: add this with a typeclass
      case Circle(c, r, d) =>
        <circle
          cx={c.real.toString}
          cy={c.imag.toString}
          r={r.toString}
            style={s"""
              opacity: ${d * TissueThin};
              stroke-opacity: ${d * TissueThin};"""} />

      case Line(p, q)        => <line x1={real(p)} y1={imag(p)} x2={real(q)} y2={imag(q)} />
      case Triangle(a, b, c) => <polygon points={s"${point(a)} ${point(b)} ${point(c)}"} />

    }
  }

  case class Line(p: Point, q: Point) extends Graphic

  case class Triangle(a: Point, b: Point, c: Point) extends Graphic

  case class Circle(center: Point, radius: Real, depth: Int) extends Graphic {

    def spawn(n: Int) =
      for (root <- Complex rootsOfUnity n)
        yield copy(center = center + root)
  }

  def emanate(n: Int)(cs: Seq[Circle]): Seq[Circle] = {

    val raws: Seq[Circle] =
      for {
        c <- cs
        e <- c spawn n if e.center.abs > c.center.abs
      } yield e

    ((raws groupBy identity) map { case (k, vs) => k copy (depth = k.depth * vs.size) }).toList

  }

  val seed = Seq(Circle(Complex(zero, zero), one, 1))

  def shells(n: Int)(len: Int): Seq[Seq[Circle]] = Seq.iterate(seed, len)(emanate(n))

  def sortByOmega(gs: Seq[Circle]) = gs sortWith { (l, r) =>
    val (_, lt) = l.center.asPolarTuple
    val (_, rt) = r.center.asPolarTuple
    lt < rt
  }

  def write(gs: Seq[Graphic]) = Files write (Logos.path, svg(gs).toString.getBytes)

  // ---

  // TODO: look at this closer - it blows up around h9 or so..
  def sortDistanceToOrigin(gs: Seq[Circle]) = gs sortWith { (l, r) =>
    val (lr, lt) = l.center.asPolarTuple
    val (rr, rt) = r.center.asPolarTuple
    lr < rr || lt < rt
  }

  protected val path = Paths get "target/logos.svg"

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def svg(gs: Seq[Graphic]) =
    <svg viewBox="-8 -8 16 16"
    xmlns:svg="http://www.w3.org/2000/svg"
    xmlns="http://www.w3.org/2000/svg">
    <style>
    /* <![CDATA[ */
    circle {
      fill: grey;
      stroke: none;
      // stroke-width: .05;
    }
    polygon {
      fill: gold;
      stroke: none;
      // stroke-width: .05;
    }
    /* ]]> */
    </style>
    <g>{ gs map (_.svg) }</g>
    </svg>

}

// scala> List.tabulate(10)(k => shells(6)(k).fold(Set.empty)(_ ++ _).size)
// res14: List[Int] = List(0, 1, 7, 19, 37, 61, 91, 127, 169, 217)

object Logos extends Logos {

  def sigilEpsilon: Seq[Graphic] = {
    val center = Complex(zero, zero)
    val ps     = Complex rootsOfUnity [Real] 6
    val male   = ps(0) * .5
    val female = male * -1
    val r      = one * .1

    Seq[Graphic](
      Triangle(ps(0), ps(1), ps(2)),
      Triangle(ps(3), ps(4), ps(5)),
      Triangle(ps(4), ps(5), ps(0)),
      Triangle(ps(0), ps(2), center),
      Triangle(ps(0), ps(4), center),
      Triangle(ps(3), ps(1), center),
      Triangle(ps(3), ps(5), center),
      Circle(male, r, 1),
      Circle(female, r, 1)
    )
  }

  object Names {
    val colors = List(
      "aqua",
      "black",
      "blue",
      "fuchsia",
      "gray",
      "green",
      "lime",
      "maroon",
      "navy",
      "olive",
      "purple",
      "red",
      "silver",
      "teal",
      "white",
      "yellow"
    )

    def rgb(red: Int, green: Int, blue: Int) = (red, green, blue)

    val cssColors = Seq(
      "aliceblue"            -> rgb(240, 248, 255),
      "antiquewhite"         -> rgb(250, 235, 215),
      "aqua"                 -> rgb(0, 255, 255),
      "aquamarine"           -> rgb(127, 255, 212),
      "azure"                -> rgb(240, 255, 255),
      "beige"                -> rgb(245, 245, 220),
      "bisque"               -> rgb(255, 228, 196),
      "black"                -> rgb(0, 0, 0),
      "blanchedalmond"       -> rgb(255, 235, 205),
      "blue"                 -> rgb(0, 0, 255),
      "blueviolet"           -> rgb(138, 43, 226),
      "brown"                -> rgb(165, 42, 42),
      "burlywood"            -> rgb(222, 184, 135),
      "cadetblue"            -> rgb(95, 158, 160),
      "chartreuse"           -> rgb(127, 255, 0),
      "chocolate"            -> rgb(210, 105, 30),
      "coral"                -> rgb(255, 127, 80),
      "cornflowerblue"       -> rgb(100, 149, 237),
      "cornsilk"             -> rgb(255, 248, 220),
      "crimson"              -> rgb(220, 20, 60),
      "cyan"                 -> rgb(0, 255, 255),
      "darkblue"             -> rgb(0, 0, 139),
      "darkcyan"             -> rgb(0, 139, 139),
      "darkgoldenrod"        -> rgb(184, 134, 11),
      "darkgray"             -> rgb(169, 169, 169),
      "darkgreen"            -> rgb(0, 100, 0),
      "darkgrey"             -> rgb(169, 169, 169),
      "darkkhaki"            -> rgb(189, 183, 107),
      "darkmagenta"          -> rgb(139, 0, 139),
      "darkolivegreen"       -> rgb(85, 107, 47),
      "darkorange"           -> rgb(255, 140, 0),
      "darkorchid"           -> rgb(153, 50, 204),
      "darkred"              -> rgb(139, 0, 0),
      "darksalmon"           -> rgb(233, 150, 122),
      "darkseagreen"         -> rgb(143, 188, 143),
      "darkslateblue"        -> rgb(72, 61, 139),
      "darkslategray"        -> rgb(47, 79, 79),
      "darkslategrey"        -> rgb(47, 79, 79),
      "darkturquoise"        -> rgb(0, 206, 209),
      "darkviolet"           -> rgb(148, 0, 211),
      "deeppink"             -> rgb(255, 20, 147),
      "deepskyblue"          -> rgb(0, 191, 255),
      "dimgray"              -> rgb(105, 105, 105),
      "dimgrey"              -> rgb(105, 105, 105),
      "dodgerblue"           -> rgb(30, 144, 255),
      "firebrick"            -> rgb(178, 34, 34),
      "floralwhite"          -> rgb(255, 250, 240),
      "forestgreen"          -> rgb(34, 139, 34),
      "fuchsia"              -> rgb(255, 0, 255),
      "gainsboro"            -> rgb(220, 220, 220),
      "ghostwhite"           -> rgb(248, 248, 255),
      "gold"                 -> rgb(255, 215, 0),
      "goldenrod"            -> rgb(218, 165, 32),
      "gray"                 -> rgb(128, 128, 128),
      "grey"                 -> rgb(128, 128, 128),
      "green"                -> rgb(0, 128, 0),
      "greenyellow"          -> rgb(173, 255, 47),
      "honeydew"             -> rgb(240, 255, 240),
      "hotpink"              -> rgb(255, 105, 180),
      "indianred"            -> rgb(205, 92, 92),
      "indigo"               -> rgb(75, 0, 130),
      "ivory"                -> rgb(255, 255, 240),
      "khaki"                -> rgb(240, 230, 140),
      "lavender"             -> rgb(230, 230, 250),
      "lavenderblush"        -> rgb(255, 240, 245),
      "lawngreen"            -> rgb(124, 252, 0),
      "lemonchiffon"         -> rgb(255, 250, 205),
      "lightblue"            -> rgb(173, 216, 230),
      "lightcoral"           -> rgb(240, 128, 128),
      "lightcyan"            -> rgb(224, 255, 255),
      "lightgoldenrodyellow" -> rgb(250, 250, 210),
      "lightgray"            -> rgb(211, 211, 211),
      "lightgreen"           -> rgb(144, 238, 144),
      "lightgrey"            -> rgb(211, 211, 211),
      "ightpink"             -> rgb(255, 182, 193),
      "lightsalmon"          -> rgb(255, 160, 122),
      "lightseagreen"        -> rgb(32, 178, 170),
      "lightskyblue"         -> rgb(135, 206, 250),
      "lightslategray"       -> rgb(119, 136, 153),
      "lightslategrey"       -> rgb(119, 136, 153),
      "lightsteelblue"       -> rgb(176, 196, 222),
      "lightyellow"          -> rgb(255, 255, 224),
      "lime"                 -> rgb(0, 255, 0),
      "limegreen"            -> rgb(50, 205, 50),
      "linen"                -> rgb(250, 240, 230),
      "magenta"              -> rgb(255, 0, 255),
      "maroon"               -> rgb(128, 0, 0),
      "mediumaquamarine"     -> rgb(102, 205, 170),
      "mediumblue"           -> rgb(0, 0, 205),
      "mediumorchid"         -> rgb(186, 85, 211),
      "mediumpurple"         -> rgb(147, 112, 219),
      "mediumseagreen"       -> rgb(60, 179, 113),
      "mediumslateblue"      -> rgb(123, 104, 238),
      "mediumspringgreen"    -> rgb(0, 250, 154),
      "mediumturquoise"      -> rgb(72, 209, 204),
      "mediumvioletred"      -> rgb(199, 21, 133),
      "midnightblue"         -> rgb(25, 25, 112),
      "mintcream"            -> rgb(245, 255, 250),
      "mistyrose"            -> rgb(255, 228, 225),
      "moccasin"             -> rgb(255, 228, 181),
      "navajowhite"          -> rgb(255, 222, 173),
      "navy"                 -> rgb(0, 0, 128),
      "oldlace"              -> rgb(253, 245, 230),
      "olive"                -> rgb(128, 128, 0),
      "olivedrab"            -> rgb(107, 142, 35),
      "orange"               -> rgb(255, 165, 0),
      "orangered"            -> rgb(255, 69, 0),
      "orchid"               -> rgb(218, 112, 214),
      "palegoldenrod"        -> rgb(238, 232, 170),
      "palegreen"            -> rgb(152, 251, 152),
      "paleturquoise"        -> rgb(175, 238, 238),
      "palevioletred"        -> rgb(219, 112, 147),
      "papayawhip"           -> rgb(255, 239, 213),
      "peachpuff"            -> rgb(255, 218, 185),
      "peru"                 -> rgb(205, 133, 63),
      "pink"                 -> rgb(255, 192, 203),
      "plum"                 -> rgb(221, 160, 221),
      "powderblue"           -> rgb(176, 224, 230),
      "purple"               -> rgb(128, 0, 128),
      "red"                  -> rgb(255, 0, 0),
      "rosybrown"            -> rgb(188, 143, 143),
      "royalblue"            -> rgb(65, 105, 225),
      "saddlebrown"          -> rgb(139, 69, 19),
      "salmon"               -> rgb(250, 128, 114),
      "sandybrown"           -> rgb(244, 164, 96),
      "seagreen"             -> rgb(46, 139, 87),
      "seashell"             -> rgb(255, 245, 238),
      "sienna"               -> rgb(160, 82, 45),
      "silver"               -> rgb(192, 192, 192),
      "skyblue"              -> rgb(135, 206, 235),
      "slateblue"            -> rgb(106, 90, 205),
      "slategray"            -> rgb(112, 128, 144),
      "slategrey"            -> rgb(112, 128, 144),
      "snow"                 -> rgb(255, 250, 250),
      "springgreen"          -> rgb(0, 255, 127),
      "steelblue"            -> rgb(70, 130, 180),
      "tan"                  -> rgb(210, 180, 140),
      "teal"                 -> rgb(0, 128, 128),
      "thistle"              -> rgb(216, 191, 216),
      "tomato"               -> rgb(255, 99, 71),
      "turquoise"            -> rgb(64, 224, 208),
      "violet"               -> rgb(238, 130, 238),
      "wheat"                -> rgb(245, 222, 179),
      "white"                -> rgb(255, 255, 255),
      "whitesmoke"           -> rgb(245, 245, 245),
      "yellow"               -> rgb(255, 255, 0),
      "yellowgreen"          -> rgb(154, 205, 50)
    )

    val cssColorMap = cssColors.toMap

    val inverseColorMap =
      cssColors
        .groupBy { case (_, v) => v }
        .map {
          case (k, vs) =>
            (k, vs.map {
              case (_, v) => v
            })
        }

    val greeks = "αβγδεζηθικλμνξοπρστυφχψω" // n.b. only one sigma

    val blackDirections: Seq[Char] = "▶▲◀▼"
    val whiteDirections: Seq[Char] = "▷△◁▽"

    val whiteUpPointingTriangleWithDot = '◬' // '\u25EC'

    val blackCircle                    = '●' // '\u25CF'
    val whiteCircle                    = '○' // '\u25CB'
    val largeBlackCircle               = '⬤' // '\u2B24
    val largeWhiteCircle               = '◯' // '\u25EF
    val fisheye                        = '◉' // '\u25C9'
    val bullseye                       = '◎' // '\u25CE'
    val antiClockWiseGappedCircleArrow = '⟲' // '\u27F2'
    val clockWiseGappedCircleArrow     = '⟳' // '\u27F3'

    val whiteDiamondContainingBlackSmallDiamond = '◈' // '\u25C8'
    val whiteSquareContainingBlackSmallSquare   = '▣' // '\u25A3'
    val blackSquare                             = '■' // '\u25A0'
    val whiteSquare                             = '□' // '\u25A1'
    val blackDiamond                            = '◆' // '\u25C6'
    val whiteDiamond                            = '◇' // '\u25C7'

    val whiteFlag = '⚐' // '\u2690'
    val blackFlag = '⚑' // '\u2691'

    val blackFourPointedStar                  = '✦' // '\u2726'
    val whiteFourPointedStar                  = '✧' // '\u2727'
    val blackStar                             = '★' // '\u2605'
    val whiteStar                             = '☆' // '\u2606'
    val stressOutlinedWhiteStar               = '✩' // '\u2729'
    val openCenterBlackStar                   = '✫' // '\u272B'
    val blackCenterWhiteStar                  = '✬' // '\u272C'
    val pinwheelStar                          = '✯' // '\u272F'
    val starOfDavid                           = '✡' // '\u2721'
    val sixPointedBlackStar                   = '✶' // '\u2736'
    val flowerPunctuationMark                 = '⁕' // '\u2055'
    val eightPointedBlackStar                 = '✴' // '\u2734'
    val eightPointedPinwheelStar              = '✵' // '\u2735'
    val eightPointedRectilinearBlackStar      = '✷' // '\u2737'
    val heavyEightPointedRectilinearBlackStar = '✸' // '\u2738'
    val circledOpenCenterEightPointedStar     = '❂' // '\u2742'
    val blackSunWithRays                      = '☀' // '\u2600'
    val whiteSunWithRays                      = '☼' // '\u263C'
    val twelvePointedBlackStar                = '✹' // '\u2739'

    val pentagons = List('\u2b1f', '\u2b20', '\u2B53', '\u2B54', '\u2BC2')

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

    val lanaSigil = "☽◯☾"

    val icHexTheCreativeHeaven        = '䷀' // U+4DC0
    val icHexTheReceptiveEarth        = '䷁' // U+4DC1
    val icHexDifficultyAtTheBeginning = '䷂' // U+4DC2
    val icHexYouthfulFolly            = '䷃' // U+4DC3
    val icHexWaiting                  = '䷄' // U+4DC4
    val icHexConflict                 = '䷅' // U+4DC5
    val icHexTheArmy                  = '䷆' // U+4DC6
    val icHexHoldingTogether          = '䷇' // U+4DC7
    val icHexSmallTaming              = '䷈' // U+4DC8
    val icHexTreading                 = '䷉' // U+4DC9
    val icHexPeace                    = '䷊' // U+4DCA
    val icHexStandstill               = '䷋' // U+4DCB
    val icHexFellowship               = '䷌' // U+4DCC
    val icHexGreatPossession          = '䷍' // U+4DCD
    val icHexModesty                  = '䷎' // U+4DCE
    val icHexEnthusiasm               = '䷏' // U+4DCF
    val icHexFollowing                = '䷐' // U+4DD0
    val icHexWorkOnTheDecayed         = '䷑' // U+4DD1
    val icHexApproach                 = '䷒' // U+4DD2
    val icHexContemplation            = '䷓' // U+4DD3
    val icHexBitingThrough            = '䷔' // U+4DD4
    val icHexGrace                    = '䷕' // U+4DD5
    val icHexSplittingApart           = '䷖' // U+4DD6
    val icHexReturn                   = '䷗' // U+4DD7
    val icHexInnocence                = '䷘' // U+4DD8
    val icHexGreatTaming              = '䷙' // U+4DD9
    val icHexMouthCorners             = '䷚' // U+4DDA
    val icHexGreatPreponderance       = '䷛' // U+4DDB
    val icHexTheAbysmalWater          = '䷜' // U+4DDC
    val icHexTheClingingFire          = '䷝' // U+4DDD
    val icHexInfluence                = '䷞' // U+4DDE
    val icHexDuration                 = '䷟' // U+4DDF
    val icHexRetreat                  = '䷠' // U+4DE0
    val icHexGreatPower               = '䷡' // U+4DE1
    val icHexProgress                 = '䷢' // U+4DE2
    val icHexDarkeningOfTheLight      = '䷣' // U+4DE3
    val icHexTheFamily                = '䷤' // U+4DE4
    val icHexOpposition               = '䷥' // U+4DE5
    val icHexObstruction              = '䷦' // U+4DE6
    val icHexDeliverance              = '䷧' // U+4DE7
    val icHexDecrease                 = '䷨' // U+4DE8
    val icHexIncrease                 = '䷩' // U+4DE9
    val icHexBreakthrough             = '䷪' // U+4DEA
    val icHexComingToMeet             = '䷫' // U+4DEB
    val icHexGatheringTogether        = '䷬' // U+4DEC
    val icHexPushingUpward            = '䷭' // U+4DED
    val icHexOppression               = '䷮' // U+4DEE
    val icHexTheWell                  = '䷯' // U+4DEF
    val icHexRevolution               = '䷰' // U+4DF0
    val icHexTheCauldron              = '䷱' // U+4DF1
    val icHexTheArousingThunder       = '䷲' // U+4DF2
    val icHexTheKeepingStillMountain  = '䷳' // U+4DF3
    val icHexDevelopment              = '䷴' // U+4DF4
    val icHexTheMarryingMaiden        = '䷵' // U+4DF5
    val icHexAbundance                = '䷶' // U+4DF6
    val icHexTheWanderer              = '䷷' // U+4DF7
    val icHexTheGentleWind            = '䷸' // U+4DF8
    val icHexTheJoyousLake            = '䷹' // U+4DF9
    val icHexDispersion               = '䷺' // U+4DFA
    val icHexLimitation               = '䷻' // U+4DFB
    val icHexInnerTruth               = '䷼' // U+4DFC
    val icHexSmallPreponderance       = '䷽' // U+4DFD
    val icHexAfterCompletion          = '䷾' // U+4DFE
    val icHexBeforeCompletion         = '䷿' // U+4DFF

    val icHexMap = Map(
      '䷀' -> "The Creative Heaven",
      '䷁' -> "The Receptive Earth",
      '䷂' -> "Difficulty At The Beginning",
      '䷃' -> "Youthful Folly",
      '䷄' -> "Waiting",
      '䷅' -> "Conflict",
      '䷆' -> "The Army",
      '䷇' -> "Holding Together",
      '䷈' -> "Small Taming",
      '䷉' -> "Treading",
      '䷊' -> "Peace",
      '䷋' -> "Standstill",
      '䷌' -> "Fellowship",
      '䷍' -> "Great Possession",
      '䷎' -> "Modesty",
      '䷏' -> "Enthusiasm",
      '䷐' -> "Following",
      '䷑' -> "Work On The Decayed",
      '䷒' -> "Approach",
      '䷓' -> "Contemplation",
      '䷔' -> "Biting Through",
      '䷕' -> "Grace",
      '䷖' -> "Splitting Apart",
      '䷗' -> "Return",
      '䷘' -> "Innocence",
      '䷙' -> "Great Taming",
      '䷚' -> "Mouth Corners",
      '䷛' -> "Great Preponderance",
      '䷜' -> "The Abysmal Water",
      '䷝' -> "The Clinging Fire",
      '䷞' -> "Influence",
      '䷟' -> "Duration",
      '䷠' -> "Retreat",
      '䷡' -> "Great Power",
      '䷢' -> "Progress",
      '䷣' -> "Darkening Of The Light",
      '䷤' -> "The Family",
      '䷥' -> "Opposition",
      '䷦' -> "Obstruction",
      '䷧' -> "Deliverance",
      '䷨' -> "Decrease",
      '䷩' -> "Increase",
      '䷪' -> "Breakthrough",
      '䷫' -> "Coming To Meet",
      '䷬' -> "Gathering Together",
      '䷭' -> "Pushing Upward",
      '䷮' -> "Oppression",
      '䷯' -> "The Well",
      '䷰' -> "Revolution",
      '䷱' -> "The Cauldron",
      '䷲' -> "The Arousing Thunder",
      '䷳' -> "The Keeping Still Mountain",
      '䷴' -> "Development",
      '䷵' -> "The Marrying Maiden",
      '䷶' -> "Abundance",
      '䷷' -> "The Wanderer",
      '䷸' -> "The Gentle Wind",
      '䷹' -> "The Joyous Lake",
      '䷺' -> "Dispersion",
      '䷻' -> "Limitation",
      '䷼' -> "Inner Truth",
      '䷽' -> "Small Preponderance",
      '䷾' -> "After Completion",
      '䷿' -> "Before Completion"
    )

// val icTetCentre = '𝌆'	//
// val icTetFullCircle = '𝌇'	//
// val icTetMired = '𝌈'	//
// val icTetBarrier = '𝌉'	//
// val icTetKeepingSmall = '𝌊'	//
// val icTetContrariety = '𝌋'	//
// val icTetAscent = '𝌌'	//
// val icTetOpposition = '𝌍'	//
// val icTetBranchingOut = '𝌎'	//
// val icTetDefectivenessOrDistortion = '𝌏'	//
// val icTetDivergence = '𝌐'	//
// val icTetYouthfulness = '𝌑'	//
// val icTetIncrease = '𝌒'	//
// val icTetPenetration = '𝌓'	//
// val icTetReach = '𝌔'	//
// val icTetContact = '𝌕'	//
// val icTetHoldingBack = '𝌖'	//
// val icTetWaiting = '𝌗'	//
// val icTetFollowing = '𝌘'	//
// val icTetAdvance = '𝌙'	//
// val icTetRelease = '𝌚'	//
// val icTetResistance = '𝌛'	//
// val icTetEase = '𝌜'	//
// val icTetJoy = '𝌝'	//
// val icTetContention = '𝌞'	//
// val icTetEndeavour = '𝌟'	//
// val icTetDuties = '𝌠'	//
// val icTetChange = '𝌡'	//
// val icTetDecisiveness = '𝌢'	//
// val icTetBoldResolution = '𝌣'	//
// val icTetPacking = '𝌤'	//
// val icTetLegion = '𝌥'	//
// val icTetCloseness = '𝌦'	//
// val icTetKinship = '𝌧'	//
// val icTetGathering = '𝌨'	//
// val icTetStrength = '𝌩'	//
// val icTetPurity = '𝌪'	//
// val icTetFullness = '𝌫'	//
// val icTetResidence = '𝌬'	//
// val icTetLawOrModel = '𝌭'	//
// val icTetResponse = '𝌮'	//
// val icTetGoingToMeet = '𝌯'	//
// val icTetEncounters = '𝌰'	//
// val icTetStove = '𝌱'	//
// val icTetGreatness = '𝌲'	//
// val icTetEnlargement = '𝌳'	//
// val icTetPattern = '𝌴'	//
// val icTetRitual = '𝌵'	//
// val icTetFlight = '𝌶'	//
// val icTetVastnessOrWasting = '𝌷'	//
// val icTetConstancy = '𝌸'	//
// val icTetMeasure = '𝌹'	//
// val icTetEternity = '𝌺'	//
// val icTetUnity = '𝌻'	//
// val icTetDiminishment = '𝌼'	//
// val icTetClosedMouth = '𝌽'	//
// val icTetGuardedness = '𝌾'	//
// val icTetGatheringIn = '𝌿'	//
// val icTetMassing = '𝍀'	//
// val icTetAccumulation = '𝍁'	//
// val icTetEmbellishment = '𝍂'	//
// val icTetDoubt = '𝍃'	//
// val icTetWatch = '𝍄'	//
// val icTetSinking = '𝍅'	//
// val icTetInner = '𝍆'	//
// val icTetDeparture = '𝍇'	//
// val icTetDarkening = '𝍈'	//
// val icTetDimming = '𝍉'	//
// val icTetExhaustion = '𝍊'	//
// val icTetSeverance = '𝍋'	//
// val icTetStoppage = '𝍌'	//
// val icTetHardness = '𝍍'	//
// val icTetCompletion = '𝍎'	//
// val icTetClosure = '𝍏'	//
// val icTetFailure = '𝍐'	//
// val icTetAggravation = '𝍑'	//
// val icTetCompliance = '𝍒'	//
// val icTetOnTheVerge = '𝍓'	//
// val icTetDifficulties = '𝍔'	//
// val icTetLabouring = '𝍕'	//
// val icTetFostering = '𝍖'	//

// val icTetCentre = '\u1D306'
// val icTetFullCircle = '\u1D307'
// val icTetMired = '\u1D308'
// val icTetBarrier = '\u1D309'
// val icTetKeepingSmall = '\u1D30A'
// val icTetContrariety = '\u1D30B'
// val icTetAscent = '\u1D30C'
// val icTetOpposition = '\u1D30D'
// val icTetBranchingOut = '\u1D30E'
// val icTetDefectivenessOrDistortion = '\u1D30F'
// val icTetDivergence = '\u1D310'
// val icTetYouthfulness = '\u1D311'
// val icTetIncrease = '\u1D312'
// val icTetPenetration = '\u1D313'
// val icTetReach = '\u1D314'
// val icTetContact = '\u1D315'
// val icTetHoldingBack = '\u1D316'
// val icTetWaiting = '\u1D317'
// val icTetFollowing = '\u1D318'
// val icTetAdvance = '\u1D319'
// val icTetRelease = '\u1D31A'
// val icTetResistance = '\u1D31B'
// val icTetEase = '\u1D31C'
// val icTetJoy = '\u1D31D'
// val icTetContention = '\u1D31E'
// val icTetEndeavour = '\u1D31F'
// val icTetDuties = '\u1D320'
// val icTetChange = '\u1D321'
// val icTetDecisiveness = '\u1D322'
// val icTetBoldResolution = '\u1D323'
// val icTetPacking = '\u1D324'
// val icTetLegion = '\u1D325'
// val icTetCloseness = '\u1D326'
// val icTetKinship = '\u1D327'
// val icTetGathering = '\u1D328'
// val icTetStrength = '\u1D329'
// val icTetPurity = '\u1D32A'
// val icTetFullness = '\u1D32B'
// val icTetResidence = '\u1D32C'
// val icTetLawOrModel = '\u1D32D'
// val icTetResponse = '\u1D32E'
// val icTetGoingToMeet = '\u1D32F'
// val icTetEncounters = '\u1D330'
// val icTetStove = '\u1D331'
// val icTetGreatness = '\u1D332'
// val icTetEnlargement = '\u1D333'
// val icTetPattern = '\u1D334'
// val icTetRitual = '\u1D335'
// val icTetFlight = '\u1D336'
// val icTetVastnessOrWasting = '\u1D337'
// val icTetConstancy = '\u1D338'
// val icTetMeasure = '\u1D339'
// val icTetEternity = '\u1D33A'
// val icTetUnity = '\u1D33B'
// val icTetDiminishment = '\u1D33C'
// val icTetClosedMouth = '\u1D33D'
// val icTetGuardedness = '\u1D33E'
// val icTetGatheringIn = '\u1D33F'
// val icTetMassing = '\u1D340'
// val icTetAccumulation = '\u1D341'
// val icTetEmbellishment = '\u1D342'
// val icTetDoubt = '\u1D343'
// val icTetWatch = '\u1D344'
// val icTetSinking = '\u1D345'
// val icTetInner = '\u1D346'
// val icTetDeparture = '\u1D347'
// val icTetDarkening = '\u1D348'
// val icTetDimming = '\u1D349'
// val icTetExhaustion = '\u1D34A'
// val icTetSeverance = '\u1D34B'
// val icTetStoppage = '\u1D34C'
// val icTetHardness = '\u1D34D'
// val icTetCompletion = '\u1D34E'
// val icTetClosure = '\u1D34F'
// val icTetFailure = '\u1D350'
// val icTetAggravation = '\u1D351'
// val icTetCompliance = '\u1D352'
// val icTetOnTheVerge = '\u1D353'
// val icTetDifficulties = '\u1D354'
// val icTetLabouring = '\u1D355'
// val icTetFostering = '\u1D356'
  }
}
