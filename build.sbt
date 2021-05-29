// resolvers += Resolver.bintrayRepo("virtuslab", "graphbuddy")
// addCompilerPlugin("com.virtuslab.semanticgraphs" % "scalac-plugin" % "0.2.2" cross CrossVersion.full)
scalacOptions += "-Yrangepos"

resolvers += Resolver.sonatypeRepo("releases")

// addCompilerPlugin("io.tryp" % "splain" % "0.5.0" cross CrossVersion.patch)
// addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full)

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

lazy val contracts = module(
  "contracts",
  """smart contract definition and evaluation"""
).settings(common)
  .settings(libraryDependencies ++= funlibs ++ testers)
  .settings( // FIXME
    libraryDependencies += catsTime
  )

lazy val localCormorant = RootProject(
  uri("https://github.com/CoinOpLLC/cormorant.git")
)

lazy val keyval = module(
  "keyval",
  """key value algebra"""
).dependsOn(localCormorant)
  .settings(common)
  .settings(libraryDependencies ++= funlibs ++ testers)
  .settings(libraryDependencies ++= fs2s ++ cormorants ++ fuuids ++ circeii)

lazy val core = module(
  "core",
  """foundational finance value types and functions"""
).dependsOn(keyval)
  .dependsOn(contracts)
  .settings(common)
  .settings(libraryDependencies ++= funlibs ++ testers)
  .settings(libraryDependencies ++= fs2s ++ cormorants ++ fuuids ++ circeii)
// https://github.com/CoinOpLLC/cormorant/tree/ce3-v3
// https://github.com/CoinOpLLC/cormorant
