/**
  *
  */
scalafmtOnCompile in ThisBuild := true // all projects

/**
  * Implementing suggestions from [this post](https://pbassiner.github.io/blog/essential_sbt_plugins.html).
  *
  * settings and tasks (mostly(!)) beginWith `dependencyUpdates`
  */
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.1")

/**
  * [Dependency graph](https://github.com/jrudolph/sbt-dependency-graph)
  */
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")

/**
  * code ccoverage plugin - https://github.com/scoverage/sbt-scoverage
  * see also https://github.com/scoverage/scalac-scoverage-plugin
  */
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

/**
  * [Dependency Checker](https://github.com/albuch/sbt-dependency-check)
  * this is a vulnerability check... use SNYK instead? or what
  */
addSbtPlugin("net.vonbuchholtz" % "sbt-dependency-check" % "0.2.1")

/**
  * Native Packager
  */
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.2")

/**
  * [License Header plugin](https://github.com/sbt/sbt-header)
  */
addSbtPlugin("de.heikoseeberger" % "sbt-header" % "4.0.0")

/**
  * As with real warts, there will be blood: be prepeared.
  * [Wart Remover](http://www.wartremover.org/)
  */
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.2.1")

/**
  * [Coursier](https://github.com/coursier/coursier#command-line)
  */
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC11")

/**
  * [source code formatter (scalafmt)](https://github.com/lucidsoftware/neo-sbt-scalafmt)
  */
addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.15")

/**
  * [The Official™ scalafmt-sbt plugin](http://scalameta.org/scalafmt/#Installation)
  */
// addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.3.0")

/**
  * [site generation](http://www.scala-sbt.org/sbt-site/index.html)
  * [but see also](https://github.com/sbt/sbt-ghpages)
  */
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.0")

/**
  * [Amazon S3 publishing and resolution](https://github.com/frugalmechanic/fm-sbt-s3-resolver)
  */
// addSbtPlugin("com.frugalmechanic" % "fm-sbt-s3-resolver" % "0.12.0")

/**
  * [scaladoc: other API resolution](https://github.com/ThoughtWorksInc/sbt-api-mappings)
  */
// addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "1.1.0") // for SBT 0.13_
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "2.0.0")

/**
  * [TUT](https://github.com/tpolecat/tut)
  */
// addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.5.5") // for SBT 0.13_
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.2")

/**
  * [tag list (TODO and friends)](https://github.com/johanandren/sbt-taglist)
  */
addSbtPlugin("com.markatta" % "sbt-taglist" % "1.4.0")

/**
  * [link](https://github.com/47deg/sbt-org-policies)
  */
// addSbtPlugin("com.47deg" % "sbt-org-policies" % "0.6.3")

/**
  * [Pretty class diagrams; needs graphviz](https://github.com/xuwei-k/sbt-class-diagram)
  */
addSbtPlugin("com.github.xuwei-k" % "sbt-class-diagram" % "0.2.1")

/**
  * [Flyway database migrations tool](https://flywaydb.org/):
  * schema roll-back, roll-forward, etc.
  */
// resolvers += "Flyway" at "https://flywaydb.org/repo"
resolvers += "Flyway" at "https://davidmweber.github.io/flyway-sbt.repo"
addSbtPlugin("org.flywaydb" % "flyway-sbt" % "4.2.0") // TODO: current version is at least 4.2.0

/**
  * [link](url)
  */
// // resolvers += Resolver.typesafeRepo("releases")
// // resolvers += Resolver.url("bintray-sbt-plugin-releases")
