package io.deftrade.sbt

import java.io.File
import java.nio.file.{ Files, Paths }
import java.sql.{ Connection, DriverManager, ResultSet }

import DepluralizerImplicit._

/**
  * Rather than "Type all the things", this generator restricts itself to typing all the indexes.
  */
object QuillCodeGen {

  type EnumModel = Vector[(String, String)] // enum -> value

  val TABLE_NAME     = "TABLE_NAME"
  val COLUMN_NAME    = "COLUMN_NAME"
  val TYPE_NAME      = "TYPE_NAME"
  val NULLABLE       = "NULLABLE"
  val PK_NAME        = "pk_name"
  val FK_TABLE_NAME  = "fktable_name"
  val FK_COLUMN_NAME = "fkcolumn_name"
  val PK_TABLE_NAME  = "pktable_name"
  val PK_COLUMN_NAME = "pkcolumn_name"

  val defaultTypeMap = Map(
    "serial"      -> "Int",
    "bigserial"   -> "Long",
    "money"       -> "BigDecimal",
    "int4"        -> "Int",
    "serial4"     -> "Int",
    "int8"        -> "Long",
    "serial8"     -> "Long",
    "float8"      -> "Double",
    "numeric"     -> "BigDecimal",
    "varchar"     -> "String",
    "text"        -> "String",
    "bool"        -> "Boolean",
    "bytea"       -> "Array[Byte]", // PostgreSQL
    "uuid"        -> "java.util.UUID", // H2, PostgreSQL
    "timestamp"   -> "LocalDateTime",
    "timestamptz" -> "OffsetDateTime",
    "interval"    -> "Duration",
    "tstzrange"   -> "Raynge[OffsetDateTime]",
    "json"        -> "Json",
    "jsonb"       -> "Json"
  )

  val logstream = System.err

  def apply(
      driver: String,
      url: String,
      user: String,
      password: String,
      pkg: String,
      imports: Seq[String],
      file: File,
      schema: String = "public",
      typeMap: Map[String, String] = defaultTypeMap,
      excludedTables: Set[String] = Set("schema_version"),
      namingStrategy: ReverseNamingStrategy = ReverseEscapingSnakeCase
  ): Unit = {

    logstream println s"Starting output generation for $file..."

    case class Table(name: String, columns: Seq[Column]) {

      def toCode: String = {

        val scalaName = namingStrategy table name

        val applyArgs = for (col <- columns)
          yield s"${col.asValueName}: ${col.scalaOptionType}"

        val valueClasses = for (c <- columns if c.isPrimaryKey)
          yield c.asValueClass

        s"""|/**
            |  * table $schema.$name
            |  */
            |case class $scalaName(
            |  ${applyArgs mkString ",\n  "}
            |)
            |object $scalaName {
            |  ${valueClasses mkString "\n  "}
            |}""".stripMargin
      }
    }

    case class ForeignKey(from: SimpleColumn, to: SimpleColumn)
    object ForeignKey {
      def apply(pair: (SimpleColumn, SimpleColumn)): ForeignKey = pair match {
        case (from, to) => ForeignKey(from, to)
      }
    }

    case class SimpleColumn(tableName: String, columnName: String) {
      def asValueType =
        s"${namingStrategy table tableName}.${namingStrategy default columnName}"
    }

    case class Column(tableName: String,
                      columnName: String,
                      scalaType: String,
                      nullable: Boolean,
                      isPrimaryKey: Boolean,
                      reference: Option[SimpleColumn]) {

      def scalaOptionType = {
        val raw = if (isPrimaryKey) {
          s"${namingStrategy table tableName}.$asTypeName"
        } else {
          reference.fold(scalaType)(_.asValueType)
        }
        if (nullable) s"Option[$raw]" else raw
      }

      lazy val asValueName: String = namingStrategy column columnName
      lazy val asTypeName: String  = namingStrategy table columnName

      def asValueClass: String =
        s"case class $asTypeName(value: $scalaType) extends AnyVal"
    }

    val startTime      = System.currentTimeMillis()
    val _jdbc          = (Class forName driver).newInstance()
    val db: Connection = DriverManager getConnection (url, user, password)

    logstream println db
    val foreignKeys: Set[ForeignKey] = {

      // construct raw `ForeignKey`s from the metadata `ResultSet`
      val unresolvedFKs = for {
        tRs  <- results(db.getMetaData getTables (null, schema, "%", Array("TABLE")))
        fkRs <- results(db.getMetaData getExportedKeys (null, schema, tRs getString TABLE_NAME))
      } yield
        ForeignKey(
          from = SimpleColumn(fkRs getString FK_TABLE_NAME, fkRs getString FK_COLUMN_NAME),
          to = SimpleColumn(fkRs getString PK_TABLE_NAME, fkRs getString PK_COLUMN_NAME)
        )

      // "resolve" by following references (?!)
      val fks = for {
        xfk <- unresolvedFKs
        yfk <- unresolvedFKs
      } yield
        if (xfk.to == yfk.from) ForeignKey(from = xfk.from, to = yfk.to)
        else xfk

      fks.toSet
    }

    val tables: Seq[Table] = {

      val tableResultSets = results(db.getMetaData getTables (null, schema, "%", Array("TABLE")))

      for {
        tRs <- tableResultSets
        if !(excludedTables contains (tRs getString TABLE_NAME))
        tableName = tRs getString TABLE_NAME
        pkNames = (results(db.getMetaData getPrimaryKeys (null, null, tableName)).toSeq map {
          _ getString COLUMN_NAME
        }).toSet
      } yield {
        val columns: Seq[Either[String, Column]] = for {
          colRs <- results(db.getMetaData getColumns (null, schema, tableName, null)).toSeq
        } yield {
          val colName  = colRs getString COLUMN_NAME
          val sqlType  = (colRs getString TYPE_NAME).toLowerCase
          val nullable = colRs getBoolean NULLABLE
          val sc       = SimpleColumn(tableName, colName)
          val ref      = foreignKeys find (_.from == sc) map (_.to)
          (typeMap get sqlType).fold(Left(sqlType): Either[String, Column]) { scalaType =>
            Right(Column(tableName, colName, scalaType, nullable, pkNames contains colName, ref))
          }
        }
        columns collect {
          case Left(colName) =>
            warn(s"UNMAPPED: table: $tableName col $colName")
        }
        Table(tableName, columns collect { case Right(col) => col })
      }
    }.force

    db.close() // all done with the db now

    val codeString =
      s"""|package ${pkg}
          |
          |${imports map (p => s"import $p") mkString "\n"}
          |
          |/**
          |  * Generated code - do not modify
          |  * QuillCodeGen
          |  * ${java.time.ZonedDateTime.now}
          |  */
          |object Tables {
          |${tables map (_.toCode) mkString "\n\n"}
          |}
       """.stripMargin

    // logstream println codeString

    // TODO: this is smelly; is there a better ideom?
    val path = file.toPath
    if (path.getParent.toFile.mkdirs) logstream println s"qcg: created dirs for $path"
    Files write (path, codeString.getBytes)
    logstream println s"Done! Wrote to ${file.toURI} (${System.currentTimeMillis() - startTime}ms)"

  }

  def results(rs: ResultSet): Stream[ResultSet] = {
    val nonEmpty = rs.first()
    // logstream println s"init: iter($rs) nonEmpty == ${nonEmpty}"
    if (nonEmpty) {
      rs.previous()
      val iter = new Iterator[ResultSet] {
        var first = true;
        def hasNext =
          first ||
            /* val ret = */ !rs.isLast()
        // logstream println s"iter($rs)::hasNext hn=${ret}"
        // ret
        def next() = {
          rs.next()
          first = false
          // val hn = !rs.isLast()
          // logstream println s"iter($rs)::next hn=${hn}"
          rs
        }
      }
      iter.toStream
    } else Stream.empty
  }

  def warn(msg: String): Unit =
    System.err.println(s"[${Console.YELLOW}warn${Console.RESET}] $msg")

  def debugPrintColumnLabels(rs: ResultSet): Unit =
    for (i <- 1 to rs.getMetaData.getColumnCount) {
      println(rs.getMetaData getColumnLabel i)
    }
}

// ---

trait ReverseNamingStrategy {

  private[this] def maybeDepluralize(s: String): String =
    s.depluralizeOption.fold(s)(identity)

  final def default(s: String): String = CamelCaser rehump s
  final def table(s: String): String   = maybeDepluralize(default(s))
  def column(s: String): String

}

object ReverseSnakeCase extends ReverseNamingStrategy {

  override def column(s: String): String = CamelCaser decap default(s)
}

object ReverseEscapingSnakeCase extends ReverseNamingStrategy {

  import CamelCaser.{ decap, escape }

  override def column(s: String): String = escape(decap(default(s)))
}

object CamelCaser {

  def rehump(s: String): String =
    (s.toLowerCase split "_" map capitalize).mkString
  def decap(s: String): String  = s.head.toLower +: s.tail
  def escape(s: String): String = if (reservedWords contains s) s"`$s`" else s

  private def capitalize(s: String): String = s match {
    case "" => "_"
    case s  => s.capitalize
  }

  private val reservedWords = Set(
    "abstract",
    "case",
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
  )

}
object DepluralizerImplicit {

  private val RxRow = "(.*)(?i:sheep|series|as|is|us)".r
  private val RxXes = "(.*)(x|ss|z|ch|sh)es".r
  private val RxIes = "(.*)ies".r
  private val RxS   = "(.*)s".r
  private val RxEn  = "(.*x)en".r

  implicit class StringDepluralizer(val s: String) extends AnyVal {

    def depluralize = s match {
      case RxRow("pizz") => s"Pizza" // exceptional Pizza
      case RxRow(_)      => s"${s}Row"
      case RxXes(t, end) => s"$t$end"
      case RxIes(t)      => s"${t}y" // parties => party
      case RxEn(t)       => t // oxen => ox
      case RxS(t)        => t // cars => car
      case _ =>
        throw new IllegalArgumentException(
          s"sorry - we seem to need a new depluralize() rule for $s"
        )
    }
    def depluralizeOption = scala.util.Try(depluralize).toOption
  }
}

//       url match {
//
//       case "" => ()
//       case _ => {
//
//         val db = Database.forConfig("postgres", ConfigFactory.parseString(s"""
//
//           postgres {
//
//             driver = "${driver}"
//             url = \"\"\"${url}\"\"\"
//             user = "${user}"
//             password = "${password}"
//
//             connectionPool = disabled
//             keepAliveConnection = true  // try shutting this off...
//           }
//           """)
//         )
//
//         val enumAction = sql"""
//             SELECT t.typname, e.enumlabel
//             FROM pg_type t JOIN pg_enum e ON t.oid = e.enumtypid;"""
//           .as[(String, String)]
//
//         val excludedTables: Seq[String] = Seq.empty // stub
//
//         val modelAction = createModel(Option(
//           defaultTables map { _ filterNot { mt => excludedTables contains mt.name } }
//         ), ignoreInvalidDefaults = false)
//
//         val future = db run (enumAction zip modelAction) map {
//           case (enumModel, schemaModel) => new QuillCodeGenerator(enumModel, schemaModel)
//         }  transform (
//           scg => scg.writeToFile("DefTradePgDriver", folder.getPath, pkg),
//           e   => throw e
//         )
//         try {
//           Await.result(future, 10 seconds)
//         }
//         finally {
//           db.close()
//         }
//       }
//     }
// }
//
// /**
//  * Generates Slick model source from the Postgres database model.
//  * This generator is specific to the def-trade project.
//  */
// class QuillCodeGenerator(enumModel: SourceCodeGenerator.EnumModel, schemaModel: slick.model.Model)
//     extends AbstractSourceCodeGenerator(schemaModel) with OutputHelpers { scg =>
//   import slick.profile.SqlProfile.ColumnOption.SqlType
//   import slick.ast.{ ColumnOption => co }
//   import Depluralizer._
//
//   override val ddlEnabled = false
//   private val RxArray = """_(.*)""".r // postgres naming convention
//   private val RxEnum = """(.*_e)""".r // project specific naming convention
//
//   /*
//    * enum model
//    */
//   val enumCode = enumModel groupBy { case (k, v) => k } map {
//     case (enum, values) => s"""
//     |object ${enum.toCamelCase} extends Enumeration with SlickPgImplicits {
//     |  type ${enum.toCamelCase} = Value
//     |  val ${values map { case (_, v) => v.toCamelCase } mkString ", "} = Value
//     |}
//     |""".stripMargin
//   } mkString
//
//   /**
//    * Derive the scala name for a row case class by depluralizing (things => thing) and
//    * converting the db names (lower_case) to scala names (camelCase).
//    */
//   override def entityName = (dbName: String) => dbName.depluralize.toCamelCase
//
//   def idType(col: m.Column): String = s"${entityName(col.table.table)}Id"
//
//   private val pkIdDefsCode = List.newBuilder[String]
//
//   import slick.model.QualifiedName
//   import scala.collection.{mutable => m}
//
//   type RepoMap = m.Map[QualifiedName, List[String]]
//   private implicit class RepoMapOps(repoMap: RepoMap) {
//     def +::=(kv: (QualifiedName, String)): RepoMap = kv match {
//       case (repo @ QualifiedName(_, _, _), code) =>
//         val codes = repoMap(repo)
//         repoMap += repo -> (code :: codes)
//     }
//   }
//   private val repositoryRefinements: RepoMap =
//     (m.Map.empty: RepoMap) withDefault (_ => Nil)
//   private val passAggRecRefinements: RepoMap =
//     (m.Map.empty: RepoMap) withDefault (_ => Nil)
//
//   // keep same conventions as slick codegen, even though I hate them.
//   type Table = TableDef
//   /**
//    * Generates source for a SQL table.
//    * @param table The [[slick.model.Table]] model
//    *
//    * Rules for Repositories
//    * - if table has a primary key, it gets a [[Repository]] instance
//    * - if it has a primary key named `id`, it gets a [[RepositoryId]] instance
//    * - if it has statusTs/endTs column pair, it gets a [[RepositoryPit]] instance
//    * - if no primary key but two foreign keys, it's a junction table
//    */
//    override def Table = new MuhTableDef(_)
//    class MuhTableDef(model: slick.model.Table) extends super.TableDef(model) { tableDef =>
//
//     import slick.{model => m}
//     val table: m.Table = model
//
//     override def mappingEnabled = true
//     override def autoIncLastAsOption = true
//
//     def isPk(col: m.Column) = col.options contains co.PrimaryKey // single col pks only
//     val pk = table.columns find isPk
//     val pkId = pk filter isId
//     val pkOther = pk filterNot isId
//
//     // compute the foreign key mapping to other tables for ID types
//     val idFkCol = (for {
//       fk <- table.foreignKeys
//       col <- fk.referencedColumns filter { c => isPk(c) && isId(c) }
//     } yield (fk.referencingColumns.head, col)).toMap
//
//
//     val tableName = TableClass.name
//     val entityName = scg.entityName(table.name.table)
//
//     var repositoryClass: String = "Repository"
//     val repositoryTraits = List.newBuilder[String]
//     val tableParents, entityParents = List.newBuilder[String]
//     val entityRefinements, tableRefinements = List.newBuilder[String]
//
//     // n.b. this collects single column indexes only
//     val indexCodes = table.indices collect {
//       case slick.model.Index(Some(name), _, Seq(col), _, _) if name.endsWith("_dk") => col
//     } map { col =>
//       val colDef = Column(col)
//       val name = colDef.rawName
//       val tpe = colDef.actualType
//       s"""
//       |  /** generated for index on $name */
//       |  def findBy${name.capitalize}($name: $tpe): DBIO[Seq[TT]] = findBy(_.$name, $name)
//       |""".stripMargin
//     }
//
//     repositoryRefinements +::= table.name -> indexCodes.mkString
//
//     // single columns of type int or long with name id MUST be primary keys by convention.
//     // by convention, primary keys of sql type int or long, which have the name "id",
//     // will get type safe, value class based wrapper types.
//     def isId(col: m.Column) =
//       col.name == "id" && // naming convention: type safe ids have this name only
//         (col.tpe == "Int" || col.tpe == "Long")
//
//     // single column primary key
//     for (col <- pkOther) {
//       val T = entityName
//       val PK = col.tpe
//       val _pk = col.name.toCamelCase.uncapitalize
//       entityParents += s"EntityPk"
//       entityRefinements ++= Seq(
//         s"type PK = $PK", s"override def _pk = ${_pk}"
//       )
//       tableParents += s"TablePk[$T]"
//       tableRefinements += s"override def _pk = ${_pk}"
//       repositoryClass = "RepositoryPk"
//       repositoryTraits += "RepositoryPkLike"
//     }
//
//     // multi-column (compound) primary key
//     // As of Slick 3.1.1, model.Table only uses the primaryKey method for compound primary keys.
//     for (pk <- table.primaryKey if pk.columns.size == 2) {
//       val PK_1 = Column(pk.columns(0)).rawType
//       val PK_2 = Column(pk.columns(1)).rawType
//       val _pk = s"""(${pk.columns map (Column(_).rawName) mkString ", "})"""
//       entityParents += s"EntityPk2"
//       entityRefinements ++= Seq(
//         s"type PK_1 = $PK_1", s"type PK_2 = $PK_2", s"override def _pk = ${_pk}"
//       )
//
//       val T = entityName
//       tableParents += s"TablePk2[$T]"
//       tableRefinements += s"override def _pk = ${_pk}"
//
//       repositoryClass = "RepositoryPk2"
//       repositoryTraits += "RepositoryPkLike"
//     }
//
//     for (fks <- table.foreignKeys) {
//       val TF = s"$entityName"
//       val EF = s"$tableName"
//       val RF = s"${tableName}Repository"
//       val fxpk = fks.referencingColumns map Column.apply match {
//         case Seq(col) => s"ef => ef.${col.name}"
//         case Seq(col0, col1) => s"ef => (ef.${col0.name}, ef.${col1.name})"
//         case wtf => throw new IllegalStateException(
//           s"unhandled foreign key mapping: ${fks.name}: $wtf"
//         )
//       }
//       repositoryRefinements +::= fks.referencedTable -> s"""
//         |implicit val ${EF.uncapitalize}Xpk: $EF => ${fks.referencedTable.table.toCamelCase}#RPK =
//         |  $fxpk
//         |lazy val ${EF.uncapitalize}XpkQuery = xpkQuery[$TF, $EF, $RF]($EF)
//         |""".stripMargin
//     }
//
//     // junction tables
//     for (pk <- table.primaryKey) {
//       val fks = table.foreignKeys
//       pk.columns match {
//         case Seq(c1, c2) if (fks contains c1) && (fks contains c2) =>
//           repositoryTraits += "RepositoryJunction"
//         case _ => ()
//       }
//     }
//
//     // type safe integral auto inc primary key - name is `id` by convention
//     for (col <- pkId) {
//       val T = entityName
//       val V = col.tpe
//       entityParents += s"EntityId"
//       entityRefinements ++= Seq(
//         s"type V = $V", s"type T = $T", s"type PK = ${idType(col)}"
//       )
//       tableParents += s"TableId[$T]"
//       repositoryClass = "RepositoryId"
//       repositoryTraits += "RepositoryPkLike"
//       pkIdDefsCode += s"""
//         |type ${idType(col)} = Id[${T}, $V]
//         |object ${idType(col)} extends IdCompanion[${T}, $V]
//         |""".stripMargin
//     }
//
//     /*
//      * point-in-time (Pit) class extensions
//      */
//     val pitSpan = table.columns find { col =>
//       col.name == "span" && // naming convention: pit range fields use this name exclusively
//         col.options.contains(SqlType("tstzrange"))
//     }
//
//     for (_ <- pitSpan) {
//       val T = entityName
//       entityParents += "EntityPit"
//       tableParents += s"TablePit[$T]"
//       repositoryTraits += "RepositoryPit"
//     }
//
//     type EntityType = EntityTypeDef
//     override def EntityType = new EntityType {
//       override def parents: Seq[String] = entityParents.result
//       override def code: String = s"""${super.code} { ${entityRefinements.result mkString "; "}}"""
//     }
//
//     type PlainSqlMapper = PlainSqlMapperDef
//     def PlainSqlMapper = new PlainSqlMapper {}
//
//     type TableClass = TableClassDef
//     override def TableClass = new TableClass {
//       override def parents: Seq[String] = tableParents.result
//       override def body: Seq[Seq[String]] = super.body :+ tableRefinements.result
//     }
//
//     type TableValue = TableValueDef
//     override def TableValue = new TableValue {
//       def mkRepositoryParents(base: String, traits: List[String]): String = {
//         val traitsDecl = traits match {
//           case Nil => ""
//           case _ => traits map (s => s"$s[$entityName, $tableName]") mkString ("with ", " with ", "")
//         }
//         s"$base[$entityName, $tableName](TableQuery[$tableName]) $traitsDecl"
//       }
//       val traits = repositoryTraits.result()
//       val parents = mkRepositoryParents(repositoryClass, traits)
//       def maybeSpanLens() = if (traits contains "RepositoryPit")
//
//       repositoryRefinements +::= table.name -> s"""
//         |override lazy val spanLens = SpanLens[$entityName](
//         |    init = { (odt, t) => t.copy(span = t.span.copy(start = Some(odt), end = None)) },
//         |    conclude = { (odt, t) => t.copy(span = t.span.copy(end = Some(odt))) })""".stripMargin
//
//       override def code: String = {
//         maybeSpanLens()
//         val repoName = s"${tableName}Repository"
//         s"""
//         |class $repoName extends $parents {
//         |  ${repositoryRefinements(table.name).mkString}
//         |}
//         |lazy val $tableName = new $repoName
//         |implicit class ${entityName}PasAggRec(val entity: $entityName)(implicit ec: ExecutionContext) extends PassiveAggressiveRecord[$entityName, $tableName, ${tableName}.type]($tableName)(ec) {
//         |  ${passAggRecRefinements(table.name).mkString}
//         |}
//         |""".stripMargin
//       }
//     }
//     type Column = ColumnDef
//     override def Column = column => new Column(column) { columnDef =>
//       /**
//        * Returns the full scala type mapped from the SQL type given in the model.
//        * Note that this method will basically make an array out of any type; if this is
//        * not supported it should not compile (fail to find implicits) when the whole model
//        * is assembled.
//        */
//       override def rawType: String = {
//         val ret = column match {
//           case _ if pkId.toSeq contains column =>
//             idType(column)
//           case _ if idFkCol.keySet contains column =>
//             idType(idFkCol(column))
//           case _ =>
//             column.options collectFirst {
//               case SqlType(pgType) =>
//                 toScala(pgType)
//             } getOrElse { throw new IllegalStateException(s"SqlType not found for $column") }
//         }
//         ret
//       }
//       private def toScala(pgType: String): String =
//         pgType match {
//         case RxArray(tn) => s"List[${toScala(tn)}]"
//         case RxEnum(en) => s"${en.toCamelCase}.${en.toCamelCase}"
//         // case "date"        => "java.time.LocalDate"
//         // case "time"        => "java.time.LocalTime"
//         // case "timestamp"   => "java.time.LocalDateTime"
//         case "timestamptz" => "java.time.OffsetDateTime"
//         // case "interval"    => "java.time.Duration"
//         // case "tsrange"   => "PgRange[java.time.LocalDateTime]"
//         case "tstzrange" => "PgRange[java.time.OffsetDateTime]"
//         case "jsonb" => "JsonString"
//         case _ => column.tpe
//       }
//       /**
//        * Project specific default values based on naming conventions.
//        * Ugly, but preferable to overriding slick model generator code.
//        */
//       override def default: Option[String] = (name, actualType) match {
//         case ("span", "PgRange[java.time.OffsetDateTime]") => Some("Span.empty")
//         case (_, "JsonString") => Some("""JsonString("{}")""")
//         case _ => super.default
//       }
//     }
//
//     type PrimaryKey = PrimaryKeyDef
//     def PrimaryKey = new PrimaryKey(_)
//
//     type ForeignKey = ForeignKeyDef
//     def ForeignKey = new ForeignKey(_) {
//       // modified to use the `rows` method to access TableQuery
//       override def code: String = {
//         val pkTable = referencedTable.TableValue.name
//         val (pkColumns, fkColumns) = (referencedColumns, referencingColumns).zipped.map { (p, f) =>
//           val pk = s"r.${p.name}"
//           val fk = f.name
//           if (p.model.nullable && !f.model.nullable) (pk, s"Rep.Some($fk)")
//           else if (!p.model.nullable && f.model.nullable) (s"Rep.Some($pk)", fk)
//           else (pk, fk)
//         }.unzip
//         s"""lazy val $name = foreignKey("$dbName", ${compoundValue(fkColumns)}, ${pkTable}.rows)(r => ${compoundValue(pkColumns)}, onUpdate=${onUpdate}, onDelete=${onDelete})"""
//       }
//     }
//     type Index = IndexDef
//     def Index = new Index(_)
//
//   }
//
//   // bake in the repository functionality
//   override def parentType = Some("Repositories")
//
//   // ensure to use our customized postgres driver at 'import profile.api._'
//   override def packageCode(profile: String, pkg: String, container: String, parentType: Option[String]): String = {
//     val tablesCode = scg.code // `Tables` is built and code generated exactly here
//     s"""package ${pkg}
//     |
//     |import scala.concurrent.ExecutionContext
//     |import com.github.tminglei.slickpg.{ Range => PgRange, JsonString }
//     |import io.deftrade.db._
//     |
//     | // AUTO-GENERATED Enumerations with Slick data model conversions
//     |
//     |$enumCode
//     |
//     |// AUTO-GENERATED Slick data model
//     |
//     |/** Stand-alone Slick data model for immediate use */
//     |object ${container} extends {
//     |  val profile = ${profile}
//     |} with ${container}
//     |
//     |/** Slick data model trait for extension (cake pattern). (Make sure to initialize this late.) */
//     |trait ${container}${parentType.map(t => s" extends $t").getOrElse("")} {
//     |  val profile: $profile  // must retain this driver
//     |  import profile.api._
//     |
//     |  // AUTO-GENERATED type-safe Int and Long primary key value classes
//     |
//     |  ${indent(pkIdDefsCode.result.mkString)}
//     |
//     |  // AUTO-GENERATED tables code
//     |
//     |  ${indent(tablesCode)}
//     |
//     |}
//     |""".stripMargin
//   }
// }
//
// /**
//  * Interprets a name as a plural noun and returns the singular version. Hacked ad hoc as needed.
//  */
// private[db] object Depluralizer {
//
//   private val RxRow = "(.*)(?i:sheep|series|as|is|us)".r
//   private val RxXes = "(.*)(x|ss|z|ch|sh)es".r
//   private val RxIes = "(.*)ies".r
//   private val RxS = "(.*)s".r
//   private val RxEn = "(.*x)en".r
//
//   implicit class StringDepluralizer(val s: String) extends AnyVal {
//
//     def depluralize = s match {
//       case RxRow("pizz") => s"Pizza" // exceptional Pizza
//       case RxRow(_) => s"${s}Row"
//       case RxXes(t, end) => s"$t$end"
//       case RxIes(t) => s"${t}y" // parties => party
//       case RxEn(t) => t // oxen => ox
//       case RxS(t) => t // cars => car
//       case _ => throw new IllegalArgumentException(
//         s"sorry - we seem to need a new depluralize() rule for $s"
//       )
//     }
//   }
// }
/**
  * Interprets a name as a plural noun and returns the singular version. Hacked ad hoc as needed.
  */
