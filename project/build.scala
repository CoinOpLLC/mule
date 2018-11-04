import sbt._
import Keys._
// import Tests._

object Version {

  val Scala          = "2.12.6"
  val Xml            = "1.1.0"
  val ScalaTest      = "3.0.5"
  val ScalaCheck     = "1.14.0"
  val TypesafeConfig = "1.3.3"
  val Cats           = "1.2.0"
  val CatsEffect     = "1.0.0"
  val Fuiid          = "0.1.2"
  val CatsTime       = "0.0.3"
  val CatsScalaCheck = "0.1.0"
  val Quicklens      = "1.4.11"
  val PureConfig     = "0.9.2"
  val Enumeratum     = "1.5.13"
  val Spire          = "0.16.0"
  val Refined        = "0.9.2"
  val Circe          = "0.8.0"
  val Cormorant      = "0.0.7"
  val Fansi          = "0.2.5"
  val Akka           = "2.5.12"
  val AkkaHttp       = "10.1.5"
  val HttpSession    = "0.5.5"
  val Ammonite       = "0.8.3"
  val Quill          = "2.3.1"
  val PgJdbc         = "9.4.1212" // FIXME // ProjectVersion.PgJdbc
  val OpenGamma      = "2.0.0"
  // val Squants        = "1.3.0"

  // val PgJdbc = "9.4-1201-jdbc41"
}

object Deps {

  import Version._

  val scompiler  = "org.scala-lang"         % "scala-compiler" % Scala
  val reflection = "org.scala-lang"         % "scala-reflect"  % Scala
  val xml        = "org.scala-lang.modules" %% "scala-xml"     % Xml
  // val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

  val conf = "com.typesafe" % "config" % TypesafeConfig

  val cats = "org.typelevel" %% "cats-core" % Cats

  val catsEffect = "org.typelevel" %% "cats-effect" % CatsEffect

  val catsTime = "io.chrisdavenport" %% "cats-time" % CatsTime

  val quicklens = "com.softwaremill.quicklens" %% "quicklens" % Quicklens

  val spire = "org.typelevel" %% "spire" % Spire

  // val squants = "org.typelevel" %% "squants" % Squants

  val opengamma = "com.opengamma.strata" % "strata-measure" % OpenGamma

  /**
    * @see https://blog.vlovgr.se/posts/2016-12-24-refined-configuration.html
    */
  val refined = (Seq(
    "eu.timepit" %% "refined",
    "eu.timepit" %% "refined-cats",
    "eu.timepit" %% "refined-pureconfig",
  ) map (_ % Refined)) :+ ("eu.timepit" %% "refined-scalacheck" % Refined % Test)

  val enumerata = Seq(
    "com.beachape" %% "enumeratum"       % Enumeratum,
    "com.beachape" %% "enumeratum-circe" % Enumeratum,
  )

  val circeii = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    // "io.circe" %% "circe-generic-extras",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-shapes",
    "io.circe" %% "circe-scodec",
    "io.circe" %% "circe-refined",
    "io.circe" %% "circe-java8",
  ) map (_ % Circe)

  /**
    * @see [Refined Configuration](https://blog.vlovgr.se/posts/2016-12-24-refined-configuration.html)
    */
  val pureConfigs = Seq(
    "com.github.pureconfig" %% "pureconfig",
    "com.github.pureconfig" %% "pureconfig-enumeratum",
    "com.github.pureconfig" %% "pureconfig-squants",
  ) map (_ % PureConfig)

  val testers = Seq(
    "org.scalatest"     %% "scalatest"       % ScalaTest,
    "org.scalacheck"    %% "scalacheck"      % ScalaCheck,
    "io.chrisdavenport" %% "cats-scalacheck" % CatsScalaCheck
  ) map (_ % Test)

  val fuiids = Seq(
    "io.chrisdavenport" %% "fuuid", // core
    "io.chrisdavenport" %% "fuuid-circe", // Circe integration
    "io.chrisdavenport" %% "fuuid-http4s", // Http4s integration
    "io.chrisdavenport" %% "fuuid-doobie" // Doobie integration
  ) map (_ % Fuiid)

  val cormorants = Seq(
    "io.chrisdavenport" %% "cormorant-core",
    "io.chrisdavenport" %% "cormorant-generic",
    "io.chrisdavenport" %% "cormorant-parser",
    "io.chrisdavenport" %% "cormorant-fs2",
    "io.chrisdavenport" %% "cormorant-http4s",
    "io.chrisdavenport" %% "cormorant-refined"
  ) map (_ % Cormorant)

  /** Marginal ergonomics and sundry whatnots – non-canon. */
  // val amm   = "com.lihaoyi" % "ammonite" % Ammonite cross CrossVersion.full
  val fansi = "com.lihaoyi" %% "fansi" % Fansi

  val postgres = "org.postgresql" % "postgresql" % PgJdbc

  val quills = Seq(
    "io.getquill" %% "quill-jdbc",
    "io.getquill" %% "quill-async-postgres",
  ) map (_ % Quill)

  /** toolkits */
  val akkaHttp   = "com.typesafe.akka" %% "akka-http"   % AkkaHttp
  val akkaActor  = "com.typesafe.akka" %% "akka-actor"  % Akka
  val akkaStream = "com.typesafe.akka" %% "akka-stream" % Akka
  // If testkit used, explicitly declare dependency on akka-streams-testkit in same version as akka-actor
  val akkaHttpTk   = "com.typesafe.akka" %% "akka-http-testkit"   % AkkaHttp % Test
  val akkaStreamTk = "com.typesafe.akka" %% "akka-stream-testkit" % Akka     % Test

  val httpSession    = "com.softwaremill.akka-http-session" %% "core" % HttpSession
  val httpSessionJwt = "com.softwaremill.akka-http-session" %% "jwt"  % HttpSession
  // -> Session[T] support: JWT, CSFR, remember-me functionality... client and server, apparently

  lazy val httplibs = List(
    akkaHttp,
    akkaActor,
    akkaStream,
    httpSession,
    httpSessionJwt
  )

  lazy val funlibs =
    List(
      cats,
      catsEffect,
      catsTime,
      quicklens,
      spire,
    ) ++ cormorants ++ fuiids

  lazy val misclibs =
    List(
      reflection,
      scompiler,
      xml, // FIXME: check the state of available alternatives
      conf,
      fansi // can't help myself
    )
}

object Args {

  lazy val alv2url = new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")

  lazy val allScalaCflags = xlintDeets ++ ywarnDeets ++ ywarnUnusedDeets ++ Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-explaintypes",
    // "-Xfatal-warnings", FIXME sick of this for now. Use for artifact level build settings.
    // "-Xlint", no! use lists of :deets
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfuture", // Turn on future language features.
    "-Yno-adapted-args",
    "-Ypartial-unification" // Enable partial unification in type constructor inference
  )

  lazy val nonConsoleScalaCflags = Set("-Xfatal-warnings") ++ ywarnUnusedDeets

  lazy val xlintDeets = List(
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match" // Pattern match may not be typesafe.
  )

  lazy val ywarnDeets = List(
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
  )
  lazy val ywarnUnusedDeets = List(
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",  // Warn if a local definition is unused.
    // "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates" // Warn if a private member is unused.
  )

  lazy val initialCommands = """"""
  // """import io.deftrade._, time._
  //   |import cats._, implicits._
  //   |
  //   |""".stripMargin
}

object License {
  def notice(
      distname: String,
      version: String,
      entity: String,
      lic: String,
      licShort: String
  ): String =
    s"""The CoinOp DefTrade distribution bundles $distname $version, copyright $entity,
  |which is available under $lic.
  |For details, see licenses/$entity-$distname.$licShort.""".stripMargin

}
