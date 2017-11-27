import Deps._

crossPaths in Global := false
cancelable in Global := true

scalafmtOnCompile := true
organization      := "com.coinopllc"
scalaVersion      := Version.Scala
organizationName  := "CoinOp LLC"
startYear         := Some(2017)
licenses          += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
headerLicense     := Some(HeaderLicense.ALv2("2017", organizationName.value))

lazy val common = List(
  scalacOptions                           ++= Args.allScalaCflags,
  scalacOptions in (Compile, console)     --= Args.nonConsoleScalaCflags.to[Seq],
  scalacOptions in (Test, console)        := (scalacOptions in (Compile, console)).value,
  wartremoverErrors in (Compile, compile) ++= Warts.unsafe,
  dependencyUpdatesFilter                 -= moduleFilter(organization = "org.scala-lang"), // #TODO... why?
  initialCommands in (Test, console)      := Args.initialCommands
)

lazy val macros = project.settings(common).settings(libraryDependencies ++= quills ++ List(reflection, scompiler, scalatest))

lazy val rdb = project.dependsOn(macros).settings(common).settings(libraryDependencies ++= moarlibs) //. // TODO: choke down on deps
// settings(
//   // scgBaseSettings(Test) ++
//   Seq(
//     flywayLocations := List("filesystem:db/src/main/resources/db/migration"),
//     flywayDriver := "org.postgresql.Driver",
//     flywayUrl := "jdbc:postgresql://localhost:5432/test",
//     flywayUser := "ndw",
//     flywayCleanOnValidationError := true,
//     flywayTable := "schema_versions" // migrations metadata table name
//     // scgPackage := "io.deftrade.db.test"
//   )
// )

// top level project - TODO: eventually this should only aggregate (no active dev)
lazy val mule = (project in file(".")).aggregate(macros, rdb).settings(common).settings(libraryDependencies ++= moarlibs)
