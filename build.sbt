resolvers += Resolver.sonatypeRepo("releases")

// addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

// addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0")

// addCompilerPlugin("io.tryp" % "splain" % "0.5.0" cross CrossVersion.patch)

scalafmtOnCompile in ThisBuild := true // all projects

import Deps._

lazy val common = Seq(
  organization                        := "io.deftrade",
  scalaVersion                        := Version.Scala,
  dependencyUpdatesFilter             -= moduleFilter(organization = "org.scala-lang"),
  crossPaths                          := false,
  cancelable                          := true,
  scalacOptions                       ++= Args.allScalaCflags,
  scalacOptions in (Compile, console) --= Args.nonConsoleScalaCflags.to[Seq],
  scalacOptions in (Test, console)    := (scalacOptions in (Compile, console)).value,
  // wartremover
  wartremoverErrors in (Compile, compile) ++= Warts.unsafe,
  // per https://issues.scala-lang.org/browse/SI-9076 - via fs2 faq
  scalacOptions in console += "-Ydelambdafy:inline",
  // scalafmt
  scalafmtOnCompile := true,
  // licence header stuff
  startYear                    := Some(2017),
  licenses                     += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  organizationName             := "CoinOp LLC",
  initialCommands in (console) := Args.initialCommands
)

def module(id: String, d: String) =
  Project(id, file(s"modules/$id"))
    .settings(moduleName := id, name := id, description := d)

lazy val core = module(
  "core",
  """foundational finance value types and functions"""
).settings(common)
  .settings(addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full))
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ doobies ++ testers
  )

lazy val demo = module("demo", "something to run")
  .dependsOn(core)
  .settings(common)
  .settings(addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full))
  .settings(
    libraryDependencies ++= funlibs ++ enumerata ++ refined ++ doobies ++ testers
  )
// .settings(
//   libraryDependencies ++= funlibs ++ enumerata ++ refined ++
//     pureConfigs ++
//     Seq(opengamma) ++
//     httplibs ++
//     testers
// )

// // wip := work in progress
// lazy val wip = module("wip", "back on my bullshit")
//   .dependsOn(core)
//   .settings(common)
//   .settings(
//     libraryDependencies ++=
//       pureConfigs ++
//         Seq(opengamma) ++
//         httplibs ++
//         testers
//   )
