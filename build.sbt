import de.heikoseeberger.sbtheader
import sbtheader.AutomateHeaderPlugin

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
  // dependencyUpdatesFilter -= moduleFilter(organization = "org.scala-lang"),
  dependencyUpdatesExclusions := moduleFilter(organization = "org.scala-lang"),
  crossPaths in Global        := false,
  cancelable in Global        := true,
  scalacOptions               ++= Args.allScalaCflags,
  scalacOptions in (Compile, console) ~=
    (_ filterNot Args.nonConsoleScalaCflags.contains),
  scalacOptions in (Test, console)        := (scalacOptions in (Compile, console)).value,
  wartremoverErrors in (Compile, compile) ++= Warts.unsafe,
  headers :=
    License(
      dates = "2017",
      entity = "Fairfax Technologies LLC"
    )
)

buildSettings

import Deps._
lazy val coreDeps = List(reflection, xml, cats)
lazy val testDeps = List(scalatest) map (_ % Test)

lazy val mule = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(
    libraryDependencies ++= coreDeps ++ testDeps
    // initialCommands in (Test, console) := """ammonite.Main().run()""",
  )
