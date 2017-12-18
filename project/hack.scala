package io.deftrade.sbt

import java.io.File
import java.nio.file.{Files, Paths}
import java.sql.{ Connection, DriverManager, ResultSet }

case class Codegen(options: CodegenOptions) {
  import Codegen._
  val columnType2scalaType = options.typeMap.pairs.toMap

  def results(resultSet: ResultSet): Iterator[ResultSet] = {
    new Iterator[ResultSet] {
      def hasNext = resultSet.next()
      def next() = resultSet
    }
  }

  def getForeignKeys(db: Connection): Set[ForeignKey] = {
    val allTables =
      results(db.getMetaData.getTables(null, options.schema, "%", Array("TABLE"))).map { row =>
        row.getString(TABLE_NAME)
      }

    allTables.flatMap { table =>
      val foreignKeys = db.getMetaData.getExportedKeys(null, options.schema, table)
      results(foreignKeys).map { row =>
        ForeignKey(
          from = SimpleColumn(
            tableName = row.getString(FK_TABLE_NAME),
            columnName = row.getString(FK_COLUMN_NAME)
          ),
          to = SimpleColumn(
            tableName = row.getString(PK_TABLE_NAME),
            columnName = row.getString(PK_COLUMN_NAME)
          )
        )
      }
    }.toSet
  }

  def warn(msg: String): Unit = {
    System.err.println(s"[${Console.YELLOW}warn${Console.RESET}] $msg")
  }

  def getTables(db: Connection, foreignKeys: Set[ForeignKey]): Seq[Table] = {
    val rs: ResultSet =
      db.getMetaData.getTables(null, options.schema, "%", Array("TABLE"))
    results(rs).flatMap { row =>
      val name = row.getString(TABLE_NAME)
      if (!excludedTables.contains(name)) {
        val columns = getColumns(db, name, foreignKeys)
        val mappedColumns = columns.filter(_.isRight).map(_.right.get)
        val unmappedColumns = columns.filter(_.isLeft).map(_.left.get)
        if (unmappedColumns.nonEmpty)
          warn(s"The following columns from table $name need a mapping: $unmappedColumns")
        Some(Table(
          name,
          mappedColumns
        ))
      } else {
        None
      }
    }.toVector
  }
  def getColumns(db: Connection,
                 tableName: String,
                 foreignKeys: Set[ForeignKey]): Seq[Either[String, Column]] = {
    val primaryKeys = getPrimaryKeys(db, tableName)
    val cols =
      db.getMetaData.getColumns(null, options.schema, tableName, null)
    results(cols).map { row =>
      val colName = cols.getString(COLUMN_NAME)
      val simpleColumn = SimpleColumn(tableName, colName)
      val ref = foreignKeys.find(_.from == simpleColumn).map(_.to)

      val typ = cols.getString(TYPE_NAME).toLowerCase
      typeMap.get(typ).map { scalaType =>
        Right(Column(
          tableName,
          colName,
          scalaType,
          cols.getBoolean(NULLABLE),
          primaryKeys contains cols.getString(COLUMN_NAME),
          ref
        ))
      }.getOrElse(Left(typ))
    }.toVector
  }

  def getPrimaryKeys(db: Connection, tableName: String): Set[String] = {
    val sb = Set.newBuilder[String]
    val primaryKeys = db.getMetaData.getPrimaryKeys(null, null, tableName)
    while (primaryKeys.next()) {
      sb += primaryKeys.getString(COLUMN_NAME)
    }
    sb.result()
  }

  def tables2code(tables: Seq[Table],
                  namingStrategy: ReverseNamingStrategy,
                  options: CodegenOptions) = {
    val body = tables.map(_.toCode).mkString("\n\n")
    s"""|package ${options.`package`}
        |${options.imports}
        |
        |//noinspection ScalaStyle
        |object Tables {
        |$body
        |}
     """.stripMargin
  }

  case class ForeignKey(from: SimpleColumn, to: SimpleColumn)

  case class SimpleColumn(tableName: String, columnName: String) {
    def toType =
      s"${namingStrategy.table(tableName)}.${namingStrategy.table(columnName)}"
  }

  case class Column(tableName: String,
                    columnName: String,
                    scalaType: String,
                    nullable: Boolean,
                    isPrimaryKey: Boolean,
                    references: Option[SimpleColumn]) {
    def scalaOptionType = makeOption(scalaType)

    def makeOption(typ: String): String = {
      if (nullable) s"Option[$typ]"
      else typ
    }

    def toType: String = this.toSimple.toType

    def toArg(namingStrategy: ReverseNamingStrategy, tableName: String): String = {
      s"${namingStrategy.column(columnName)}: ${makeOption(this.toType)}"
    }

    def toSimple = references.getOrElse(SimpleColumn(tableName, columnName))

    def toClass: String = {
      s"case class ${namingStrategy.table(columnName)}(value: $scalaType) extends AnyVal"
    }
  }

  case class Table(name: String, columns: Seq[Column]) {
    def toCode: String = {
      val scalaName = namingStrategy.table(name)
      val args = columns.map(_.toArg(namingStrategy, scalaName)).mkString(", ")
      val applyArgs = columns.map { column =>
        s"${namingStrategy.column(column.columnName)}: ${column.scalaOptionType}"
      }.mkString(", ")
      val applyArgNames = columns.map { column =>
        val typName = if (column.references.nonEmpty) {
          column.toType
        } else {
          namingStrategy.table(column.columnName)
        }
        if (column.nullable) {
          s"${namingStrategy.column(column.columnName)}.map($typName.apply)"
        } else {
          s"$typName(${namingStrategy.column(column.columnName)})"
        }
      }.mkString(", ")
      val classes =
        columns.withFilter(_.references.isEmpty).map(_.toClass).mkString("\n")

      s"""|  /////////////////////////////////////////////////////
          |  // $scalaName
          |  /////////////////////////////////////////////////////
          |case class $scalaName($args)
          |object $scalaName {
          |  def create($applyArgs): $scalaName = {
          |    $scalaName($applyArgNames)
          |  }
          |$classes
          |}""".stripMargin
    }
  }
}



// ---

trait ReverseNamingStrategy extends io.getquill.ReverseNamingStrategy

object ReverseSnakeCase extends ReverseNamingStrategy {

  import CamelCaser.{rehump, decap}

  override def column(s: String): String = decap(rehump(s))
  override def default(s: String): String = rehump(s)
}

object ReverseEscapingSnakeCase extends ReverseNamingStrategy {

  import CamelCaser.{rehump, decap, escape}

  override def column(s: String): String = escape(decap(rehump(s)))
  override def default(s: String): String = rehump(s)
}

object CamelCaser {

  def rehump(s: String): String = (s.toLowerCase split "_" map capitalize).mkString
  def decap(s: String): String = s.head.toLower +: s.tail
  def escape(s: String): String = if (reservedWords contains s)) s"`$s`" else s

  private def capitalize(s: String): String = s match {
    case "" => "_"
    case s => s.capitalize
  }

  private val reservedWords = Set(
    "abstract", "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield",

}
