// package io.deftrade.sbt

import sbt._
import Keys._

import java.io.File

object QuillCodeGenPlugin extends AutoPlugin {

  override def requires = org.flywaydb.sbt.FlywayPlugin
  override def trigger  = noTrigger

  object autoImport {

    lazy val qcgPackage = settingKey[String](
      "package for generated tables and repos."
    )

    lazy val qcgImports = settingKey[Seq[String]](
      "list of import statements to add to generated source file"
    )

    lazy val qcgOutFileName = settingKey[String](
      "file name for output from code generator"
    )

    lazy val qcgOutFile = settingKey[File](
      "output File for code generator; derives from qcgPackage and qcgOutFileName by default"
    )

    lazy val qcgRunUncached = taskKey[Seq[File]](
      "run the code generator unconditionally"
    )
    lazy val qcgRun = taskKey[Seq[File]](
      "run the code generator if db evolution will trigger based on input set"
    )
    def qcgBaseSettings: Seq[Setting[_]] = {

      import org.flywaydb.sbt.FlywayPlugin.autoImport._

      Seq(
        qcgPackage := "io.deftrade.rdb",
        qcgImports := Seq(
          // "io.getquill._",
          // "cats.syntax._",
          // "cats.implicits._",
          // "java.util.UUID",
          "java.time.{LocalDateTime, OffsetDateTime, Duration}",
          "enumeratum.{Enum, EnumEntry}",
          "io.circe.Json"
        ),
        qcgOutFileName := "GeneratedQuillCode.scala",
        qcgOutFile := ((qcgPackage.value split '.')
          .foldLeft(sourceManaged.value / "scala") { _ / _ }) /
          qcgOutFileName.value,
        qcgRunUncached := {

          // flywayMigrate.value // i.e. do the migrate. Returns Unit - pure effect

          _root_.io.deftrade.sbt.QuillCodeGen(
            log = streams.value.log,
            driver = flywayDriver.value, // conciously coupling to Flyway config
            url = flywayUrl.value,
            user = flywayUser.value,
            password = flywayPassword.value,
            pkg = qcgPackage.value,
            imports = qcgImports.value,
            file = qcgOutFile.value,
          )
          Seq(qcgOutFile.value)
        },
        qcgRun := {

          import FilesInfo.{ exists, lastModified } // these are the flags we care about

          type FileSetXform      = Set[File] => Set[File]
          type FileSetCombinator = FileSetXform => FileSetXform

          // named; sbt forbids invoking tasks within anon functions
          def doTheThing: Set[File] = {
            qcgRunUncached.value
            Set(qcgOutFile.value)
          }

          val inSet = Set.empty[File] ++ (flywayLocations.value map { fl =>
            (fl.stripPrefix("filesystem:") split '/')
              .foldLeft(baseDirectory.value) { _ / _ }
          })
          val tag = streams.value.cacheDirectory / "gen-quill-code"
          val cache: FileSetCombinator =
            FileFunction.cached(tag, lastModified, exists)
          val cachedQcg: FileSetXform = cache { _ => // n.b. we use inSet only to trigger
            doTheThing
          }

          cachedQcg(inSet).toSeq
        },
        sourceGenerators += qcgRun.taskValue
      )
    }

    // val wut = sql"""
    //         SELECT t.typname, e.enumlabel
    //         FROM pg_type t JOIN pg_enum e ON t.oid = e.enumtypid;"""
    //       .as[(String, String)].toMap

  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] =
    inConfig(Compile)(qcgBaseSettings)

}
