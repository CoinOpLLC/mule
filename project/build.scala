import sbt._
import Keys._
// import Tests._

object Version {
  val Scala          = "2.12.3"
  val Xml            = "1.0.6"
  val TypesafeConfig = "1.3.1"
  val ScalaTest      = "3.0.4"
  val Cats           = "0.9.0"
  val Ammonite       = "0.8.3"
  val PureConfig     = "0.8.0"
  val Enumeratum     = "1.5.12"
  val Spire          = "0.14.1"
  val Refined        = "0.8.0"
}

object Deps {

  import Version._

  val reflection = "org.scala-lang"         % "scala-reflect" % Scala
  val xml        = "org.scala-lang.modules" %% "scala-xml"    % Xml
  // val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

  val conf = "com.typesafe" % "config" % TypesafeConfig

  val cats = "org.typelevel" %% "cats" % Cats

  val spire = "org.typelevel" %% "spire" % Spire

  /**
    * @see https://blog.vlovgr.se/posts/2016-12-24-refined-configuration.html
    */
  val refined = "eu.timepit" %% "refined" % Refined

  val enumerata = Seq(
    "com.beachape" %% "enumeratum" % Enumeratum,
    "com.github.pureconfig" %% "pureconfig-enumeratum" % PureConfig
  )


  val amm = "com.lihaoyi" % "ammonite" % Ammonite cross CrossVersion.full

  /**
    * @see [Refined Configuration](https://blog.vlovgr.se/posts/2016-12-24-refined-configuration.html)
    */
  val pureConfigs = Seq(
    "com.github.pureconfig" %% "pureconfig"         % PureConfig,
    "eu.timepit"            %% "refined-pureconfig" % Refined
  )

  val scalatest = "org.scalatest" %% "scalatest" % ScalaTest
  val scalactic = "org.scalactic" %% "scalactic" % ScalaTest // sic - versions track
}

object Args {
  lazy val allScalaCflags = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-explaintypes",
    // "-Xfatal-warnings", sick of this for now. Use for artifact level build settings.
    // "-Xlint", no! use lists of :deets
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfuture", // Turn on future language features.
    "-Yno-adapted-args",
    "-Ypartial-unification" // Enable partial unification in type constructor inference
  ) ++ xlintDeets ++ ywarns ++ ywarnUnusedDeets

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

  lazy val ywarns = List(
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
}
