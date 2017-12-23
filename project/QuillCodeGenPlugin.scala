package io.deftrade.sbt

import sbt._
import Keys._

import java.io.File

object QuillCodeGenPlugin extends AutoPlugin {

  override def requires = org.flywaydb.sbt.FlywayPlugin
  // override def trigger = allRequirements

  type FileSet = Set[File]
  type FileSetFunction = FileSet => FileSet
  type FileSetCombinator = FileSetFunction => FileSetFunction

  object autoImport {

    lazy val qcgPackage = settingKey[String](
      "package for generated tables and repos."
    )
    lazy val qcgOutFile = settingKey[File](
      "output file from code generator"
    )
    lazy val qcgRunUncached = taskKey[Seq[File]](
      "run the code generator unconditionally"
    )
    lazy val qcgRun = taskKey[Seq[File]](
      "run the code generator if db evolution will trigger based on input set"
    )
    def qcgBaseSettings(conf: Configuration): Seq[Setting[_]] = {
      import org.flywaydb.sbt.FlywayPlugin.autoImport._

      Seq(
        qcgPackage := "rdb",
        qcgOutFile := {
          ((qcgPackage.value split '.')
            .foldLeft((sourceManaged in conf).value) { _ / _ }) /
            QuillCodeGen.scalaFileName
        },
        qcgRunUncached := {
          flywayMigrate.value // i.e. do the migrate. Returns Unit - pure effect
          QuillCodeGen(
            driver = flywayDriver.value, // conciously coupling to Flyway config
            url = flywayUrl.value,
            user = flywayUser.value,
            password = flywayPassword.value,
            file = (sourceManaged in conf).value, // n.b. `sourceManaged` => treat as disposable
            pkg = qcgPackage.value
          )
          Seq(qcgOutFile.value)
        },
        qcgRun := {

          import FilesInfo.{
            exists,
            lastModified
          } // these are the flags we care about

          // named; sbt forbids invoking tasks within anon functions
          def doTheThing: FileSet = {
            qcgRunUncached.value
            Set(qcgOutFile.value)
          }

          val inSet = Set.empty[File] ++ (flywayLocations.value map { fl =>
            (fl.stripPrefix("filesystem:") split '/')
              .foldLeft(baseDirectory.value) { _ / _ }
          })
          val tag = streams.value.cacheDirectory / "gen-quill-code"
          val cacher: FileSetCombinator =
            FileFunction.cached(tag, lastModified, exists)
          val cachedQcg: FileSetFunction = cacher(_ => doTheThing)

          cachedQcg(inSet).toSeq
        },
        sourceGenerators in conf += qcgRun.taskValue
      )
    }
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = qcgBaseSettings(Compile)

}
