import Deps._

lazy val common = Seq(
  organization                            := "io.deftrade",
  organizationName                        := "CoinOp LLC",
  scalaVersion                            := Version.Scala,
  scalacOptions                           ++= Args.allScalaCflags,
  scalacOptions in (Compile, console)     --= Args.nonConsoleScalaCflags.to[Seq],
  scalacOptions in (Test, console)        := (scalacOptions in (Compile, console)).value,
  wartremoverErrors in (Compile, compile) ++= Warts.unsafe,
  dependencyUpdatesFilter                 -= moduleFilter(organization = "org.scala-lang"),
  crossPaths                              := false,
  cancelable                              := true,
  scalafmtOnCompile                       := true,
  initialCommands in (Test, console)      := Args.initialCommands,
  ensimeIgnoreMissingDirectories          := true,
  startYear                               := Some(2017),
  licenses                                += ("Apache-2.0", Args.alv2url),
  headerLicense := Some(
    HeaderLicense.ALv2("2017", organizationName.value)
  )
)

lazy val flyway = Seq(
  flywayDriver   := "org.postgresql.Driver",
  flywayUrl      := "jdbc:postgresql://localhost:5432/test",
  flywayUser     := "deftrade",
  flywayPassword := "password"
)

lazy val macros = project
  .settings(common)
  .settings(
    libraryDependencies ++= quills ++
      Seq(reflection, scompiler, scalatest)
  )

lazy val rdb = project
  .dependsOn(macros)
  .enablePlugins(QuillCodeGenPlugin)
  .settings(common)
  .settings(flyway)
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ pureConfigs ++ quills ++ misclibs ++
      Seq(postgres)
  )

lazy val wip = project
  .dependsOn(
    macros
    // rdb,
  )
  .settings(common)
  .settings(
    libraryDependencies ++=
      funlibs ++ enumerata ++ refined ++ pureConfigs ++ misclibs ++
        quills ++ httplibs ++ circeii ++
        Seq(postgres)
  )
// .settings(
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
lazy val mule = (project in file("."))
  .aggregate(macros, rdb, wip)
// .settings(common)
