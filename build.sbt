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
    HeaderLicense.ALv2("2017", organizationName.value) // FIXME: why repeat the year?
  )
)

lazy val flyway = Seq(
  flywayCleanOnValidationError := true,
  flywayTable                  := "schema_versions", // migrations metadata table name
  flywayLocations              := Seq("filesystem:rdb/src/main/resources/db/migration"),
  flywayDriver                 := "org.postgresql.Driver",
  flywayUrl                    := "jdbc:postgresql://localhost:5432/test",
  flywayUser                   := "deftrade",
  flywayPassword               := "password"
)

lazy val macros = project
  .settings(common)
  .settings(
    libraryDependencies ++= quills ++
      Seq(reflection, scompiler, scalatest)
  )

lazy val core = project
  .dependsOn(macros)
  .settings(common)
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ pureConfigs ++ misclibs
  )

// rdb := relational data base
lazy val rdb = project
  .dependsOn(core)
  .enablePlugins(QuillCodeGenPlugin)
  .settings(common, flyway)
  .settings(
    libraryDependencies ++= quills ++ circeii ++ Seq(postgres)
  )

// wip := work in progress
lazy val wip = project
// .dependsOn(core, rdb)
  .dependsOn(core)
  .settings(common)
  .settings(
    libraryDependencies ++= httplibs ++ Seq(scalatest)
  )

// top level project - TODO: eventually this should only aggregate (no active dev)
lazy val mule = (project in file("."))
// .dependsOn(core, rdb, wip)
  .dependsOn(core, rdb)
  .settings(common)
