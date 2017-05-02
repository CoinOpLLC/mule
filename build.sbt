import de.heikoseeberger.sbtheader
import sbtheader.AutomateHeaderPlugin
import sbtheader.license.Apache2_0

def latestScalafmt = "0.7.0-RC1"
commands += Command.args("scalafmt", "Run scalafmt cli.") {
  case (state, args) =>
    val Right(scalafmt) =
      org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion(latestScalafmt)
    scalafmt.main("--non-interactive" +: args.toArray)
    state
}

lazy val buildSettings = List(
  organization := "io.deftrade",
  scalaVersion := Version.Scala,
  crossPaths in Global := false,
  cancelable in Global := true,

  // scalacOptions in Compile := Seq("-deprecation", "-feature", "-Xlint"),
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-Yno-adapted-args",
    "-Ywarn-value-discard",
    "-Xlint",
    "-Xfatal-warnings",
    "-unchecked"
  ),
  scalacOptions in Compile ++= Seq(
    // "-Yno-imports",
    "-Ywarn-numeric-widen"
  ),
  // scalacOptions in Compile := Args.tpolecatScalaC,
  headers := Map(
    "scala" -> Apache2_0("2017", "Fairfax Technologies LLC"),
    "conf"  -> Apache2_0("2017", "Fairfax Technologies LLC", "#")))

buildSettings

import Deps._
lazy val coreDeps = List(reflection, xml, cats)
lazy val testDeps = List(scalatest) map (_ % Test)

lazy val mule = (project in file(".")).
  enablePlugins(AutomateHeaderPlugin).
  settings(
    libraryDependencies ++= coreDeps ++ testDeps
    // initialCommands in (Test, console) := """ammonite.Main().run()""",
  )
