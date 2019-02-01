resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

import Deps._

lazy val common = Seq(

  organization     := "io.deftrade",
  organizationName := "CoinOp LLC",

  scalaVersion     := Version.Scala,

  // scalaVersion      := "2.12.4-bin-typelevel-4",
  // scalaOrganization := "org.typelevel",
  // scalacOptions                           += "-Yliteral-types",

  scalacOptions                           ++= Args.allScalaCflags,
  scalacOptions in (Compile, console)     --= Args.nonConsoleScalaCflags.to[Seq],
  scalacOptions in (Test, console)        := (scalacOptions in (Compile, console)).value,

  wartremoverErrors in (Compile, compile) ++= Warts.unsafe,

  // per https://issues.scala-lang.org/browse/SI-9076 - via fs2 faq
  scalacOptions in console     += "-Ydelambdafy:inline",

  dependencyUpdatesFilter      -= moduleFilter(organization = "org.scala-lang"),
  crossPaths                   := false,
  cancelable                   := true,

  scalafmtOnCompile            := true,

  startYear := Some(2017),
  licenses  += ("Apache-2.0", Args.alv2url),
  headerLicense := Some(
    HeaderLicense.ALv2("2017", organizationName.value) // FIXME: why repeat the year?
  ),

  initialCommands in (console) := Args.initialCommands,
)

def module(id: String, d: String): Project =
  Project(id, file(s"modules/$id"))
    .settings(moduleName := id, name := id, description := d)

lazy val macros = module("macros", "macros go here")
  .settings(common)
  .settings(
    libraryDependencies ++= testers ++
      Seq(reflection, scompiler)
  )

lazy val core = module("core", "identity, time, and money, Scalazzi style")
  .dependsOn(macros)
  .settings(common)
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ misclibs ++ testers
  )

lazy val model = module("model", "foundational finance VOs/enums/fns/MTLs")
  .dependsOn(core)
  .settings(common)
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ pureConfigs ++ misclibs ++ testers
  )

lazy val api = module("api", "domain entities as aecore/cqrs-es instances")
  .dependsOn(model)
  .settings(common)
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ pureConfigs ++ misclibs ++ testers
  )

lazy val demo = module("demo", "something to run")
  .dependsOn(api)
  .settings(common)
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ pureConfigs ++ misclibs ++ testers
  )

// wip := work in progress
lazy val wip = module("wip", "back on my bullshit")
  .dependsOn(api)
  .settings(common)
  .settings(
    libraryDependencies ++= httplibs ++ testers ++ Seq(opengamma)
  )

// top level project - TODO: eventually this should only  (no active dev)
lazy val deftrade = (project in file("."))
  .dependsOn(macros, core, model, api, demo, wip)

  // lazy val flyway = Seq(
  //   flywayCleanOnValidationError := true,
  //   flywayTable                  := "schema_versions", // migrations metadata table name
  //   flywayLocations              := Seq("filesystem:rdb/src/main/resources/db/migration"),
  //   flywayDriver                 := "org.postgresql.Driver",
  //   flywayUrl                    := "jdbc:postgresql://localhost:5432/test",
  //   flywayUser                   := "deftrade",
  //   flywayPassword               := "password"
  // )

  // lazy val rdb = project
  //   .dependsOn(core)
  //   .enablePlugins(QuillCodeGenPlugin)
  //   .settings(common, flyway)
  //   // .settings(common)
  //   .settings(
  //     libraryDependencies ++= quills ++ circeii ++ Seq(postgres)
  //   )
