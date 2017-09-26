/**
  * Implementing suggestions from [this post](https://pbassiner.github.io/blog/essential_sbt_plugins.html).
  */
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.1")

/**
  * [Dependency graph](https://github.com/jrudolph/sbt-dependency-graph)
  */
// addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

/**
  * code ccoverage plugin not supported yet... !
  */
// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

/**
  * [Dependency Checker](https://github.com/albuch/sbt-dependency-check)
  * this is a vulnerability check... use SNYK instead? or what
  */
addSbtPlugin("net.vonbuchholtz" % "sbt-dependency-check" % "0.1.10")

/**
  * Native Packlager
  */
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.2")

/**
  * [License Header plugin](https://github.com/sbt/sbt-header)
  */
addSbtPlugin("de.heikoseeberger" % "sbt-header" % "3.0.1")

/**
  * As with real warts, there will be blood: be prepeared.
  * [Wart Remover](http://www.wartremover.org/)
  */
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.2.0")

/**
  * [Coursier](https://github.com/coursier/coursier#command-line)
  */
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC11")

/**
  * [source code formatter (scalafmt)](https://github.com/lucidsoftware/neo-sbt-scalafmt)
  */
addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.11")

/**
  * [site generation](http://www.scala-sbt.org/sbt-site/index.html)
  * [but see also](https://github.com/sbt/sbt-ghpages)
  */
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.0")

/**
  * [Amazon S3 publishing and resolution](https://github.com/frugalmechanic/fm-sbt-s3-resolver)
  */
addSbtPlugin("com.frugalmechanic" % "fm-sbt-s3-resolver" % "0.12.0")

/**
  * [scaladoc: other API resolution](https://github.com/ThoughtWorksInc/sbt-api-mappings)
  */
// addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "2.0.0")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "1.1.0")

/**
  * [TUT](https://github.com/tpolecat/tut)
  */
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.5.5")

/**
  * [tag list (TODO and friends)](https://github.com/johanandren/sbt-taglist)
  */
addSbtPlugin("com.markatta" % "sbt-taglist" % "1.4.0")

/**
  * [link](https://github.com/47deg/sbt-org-policies)
  */
addSbtPlugin("com.47deg" % "sbt-org-policies" % "0.6.3")

/**
  * [link](url)
  */

// #TODO: https://github.com/xuwei-k/sbt-class-diagram
