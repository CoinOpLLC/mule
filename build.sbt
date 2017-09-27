// import de.heikoseeberger.sbtheader
// import sbtheader.AutomateHeaderPlugin

// def latestScalafmt = "0.7.0-RC1"
// commands += Command.args("scalafmt", "Run scalafmt cli.") {
//   case (state, args) =>
//     val Right(scalafmt) =
//       org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion(latestScalafmt)
//     scalafmt.main("--non-interactive" +: args.toArray)
//     state
// }

val apache2URL = new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")

lazy val muhBuildSettings = List(

  organization            := "com.coinopllc",

  scalaVersion            := Version.Scala,
  dependencyUpdatesFilter -= moduleFilter(organization = "org.scala-lang"), // #TODO... why?

  crossPaths in Global    := false,
  cancelable in Global    := true,

  scalacOptions           ++= Args.allScalaCflags,
  scalacOptions in (Compile, console) --= Args.nonConsoleScalaCflags.to[Seq],
  scalacOptions in (Test, console)        := (scalacOptions in (Compile, console)).value,

  wartremoverErrors in (Compile, compile) ++= Warts.unsafe,

  organizationName                        := "CoinOp LLC",
  startYear                               := Some(2017),
  licenses                                += ("Apache-2.0", apache2URL)
)

muhBuildSettings

lazy val coreDeps = Deps.common
lazy val testDeps = List(Deps.scalatest) map (_ % Test)

lazy val mule = (project in file("."))
// .enablePlugins(AutomateHeaderPlugin)
  .settings(
    libraryDependencies ++= coreDeps ++ testDeps,
    initialCommands in (Test, console) := Args.initialCommands
  )
