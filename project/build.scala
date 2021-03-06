import sbt._
import Keys._
// import Tests._

object Deps {

  object Version {

    val Scala = "2.13.6"
    val Xml = "1.2.0"

    val Cats       = "2.6.0"
    val Kittens    = "2.3.0"

    val CatsEffect = "3.1.1"
    val Fs2        = "3.0.4"

    val Spire     = "0.17.0"
    val Shapeless = "2.3.7"
    val Refined   = "0.9.25"

    val Enumeratum = "1.6.1"

    val Fuiid      = "0.6.0"
    val Cormorant = "0.3.0" // "0.6.0"

    val Circe          = "0.14.1"
    val PureConfig     = "0.15.0"
    val TypesafeConfig = "1.4.1"

    // val Http4s = "1.0.0-M8" // "0.21.4"
    // val Doobie = "0.9.0"
    //
    // val Slf4j          = "1.7.30"

    // val Fansi = "0.2.7"
    // val OpenGamma = "2.3.2"

    // Test libs

    val ScalaTest  = "3.2.6"
    val ScalaCheck = "1.15.3"
    val ScalaTestPlusScalaCheck = "3.2.7.0"
    val CatsScalaCheck      = "0.2.0"
    val ShapelessScalaCheck = "1.2.5"
    val CatsTime = "0.3.4"
  }

  import Version._

  val scompiler  = "org.scala-lang"          % "scala-compiler" % Scala
  val reflection = "org.scala-lang"          % "scala-reflect"  % Scala
  val xml        = "org.scala-lang.modules" %% "scala-xml"      % Xml
  // val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  lazy val misclibs =
    List(
      reflection,
      scompiler,
      xml,  // FIXME: check the state of available alternatives
    )

  val typelevel = Seq(
    "org.typelevel" %% "cats-core" % Cats,
    "org.typelevel" %% "cats-free" % Cats,
    "org.typelevel" %% "kittens" % Kittens,
    "org.typelevel" %% "cats-effect" % CatsEffect,
    "org.typelevel" %% "spire" % Spire,
    "com.chuusai" %% "shapeless" % Shapeless,
  )

  /** @see https://blog.vlovgr.se/posts/2016-12-24-refined-configuration.html
    */
  val refined =
    (
      Seq(
        "",
        "-eval",
        "-cats",
        "-pureconfig",
        "-shapeless"
      ) map (x => "eu.timepit" %% s"refined$x" % Refined)
    ) ++
      Seq("eu.timepit" %% "refined-scalacheck" % Refined % Test)

  val enumerata = Seq(
    "com.beachape" %% "enumeratum"       % Enumeratum,
    "com.beachape" %% "enumeratum-cats"  % Enumeratum, // "1.5.16",
    "com.beachape" %% "enumeratum-circe" % Enumeratum // "1.5.22",
  )

  val circeii = Seq(
    "core",
    "generic",
    "parser",
    "shapes",
    "scodec",
    "literal",
    "refined"
  ) map { x =>
    "io.circe" %% s"circe-$x" % Circe
  }

  /** @see https://blog.vlovgr.se/posts/2016-12-24-refined-configuration.html
    */
  val pureConfigs = List(
    "pureconfig",
    "pureconfig-enumeratum",
    "pureconfig-fs2",
    "pureconfig-http4s",
    "pureconfig-cats",
    "pureconfig-cats-effect",
    "pureconfig-circe",
    "pureconfig-yaml"
  ) map (x => "com.github.pureconfig" %% x % PureConfig)

  val fuuids = Seq(
    "fuuid",       // core
    "fuuid-circe", // Circe integration
    "fuuid-http4s" // Http4s integration
    // "fuuid-doobie" // Doobie integration
  ) map (x => "io.chrisdavenport" %% s"$x" % Fuiid)

  val cormorants = Seq(
    "core",
    "generic",
    "parser",
    "refined",
    "fs2",
    "http4s"
  ) map (x => "io.chrisdavenport" %% s"cormorant-$x" % Cormorant)

  val fs2s = Seq(
    "co.fs2" %% "fs2-core",
    "co.fs2" %% "fs2-io"
    // "co.fs2" %% "fs2-reactive-streams",
    // "co.fs2" %% "fs2-experimental",
  ) map (_ % Fs2)

  val testers = Seq(
    "org.scalatest"              %% "scalatest"                 % ScalaTest,
    "org.scalacheck"             %% "scalacheck"                % ScalaCheck,
    "org.scalatestplus"          %% "scalacheck-1-15"           % ScalaTestPlusScalaCheck,
    "io.chrisdavenport"          %% "cats-scalacheck"           % CatsScalaCheck,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % ShapelessScalaCheck,
    "io.circe"                   %% "circe-testing"             % Circe,
  ) map (_ % Test)

  val catsTime = "io.chrisdavenport" %% "cats-time" % CatsTime


  /** Marginal ergonomics and sundry whatnots – non-canon. */
  // val amm   = "com.lihaoyi" % "ammonite" % Ammonite cross CrossVersion.full
  // val fansi = "com.lihaoyi" %% "fansi" % Fansi
  // val opengamma = "com.opengamma.strata" % "strata-measure" % OpenGamma

  lazy val funlibs = typelevel ++ enumerata ++ refined
    // fs2s ++
    // cormorants ++
    // fuuids ++
    // circeii
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
    "-Xcheckinit" // Wrap field accessors to throw an exception on uninitialized access.
    // "-Xfuture", // Turn on future language features.
    // "-Yno-adapted-args",
    // "-Yliteral-types",
    // "-Ypartial-unification" // Enable partial unification in type constructor inference
  )

  lazy val nonConsoleScalaCflags = Set("-Xfatal-warnings") ++ ywarnUnusedDeets

  lazy val xlintDeets = List(
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    // "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant",             // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",   // Selecting member of DelayedInit.
    "-Xlint:doc-detached",         // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",         // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",            // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    // "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",        // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow"   // A local type parameter shadows a type already in scope.
    // "-Xlint:unsound-match" // Pattern match may not be typesafe.
  )

  lazy val ywarnDeets = List(
    "-Ywarn-dead-code",      // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    // "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    // "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    // "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    // "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-value-discard"  // Warn when non-Unit expression results are unused.
  )
  lazy val ywarnUnusedDeets = List(
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",   // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",    // Warn if a local definition is unused.
    // "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates" // Warn if a private member is unused.
  )

  lazy val initialCommands = """"""
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
