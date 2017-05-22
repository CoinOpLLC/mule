/**
  * Implwmwnra suggestions from [this post](https://pbassiner.github.io/blog/essential_sbt_plugins.html).
  */
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M9")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.8.0")

// http://www.wartremover.org/
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.0.3")
