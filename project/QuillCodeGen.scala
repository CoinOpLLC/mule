// package io.deftrade.sbt
//
// import java.io.File
// import java.nio.file.{ Files, Paths }
// import java.sql.{ Connection, DriverManager, ResultSet }
//
// import _root_.sbt.Logger
//
// import DepluralizerImplicit._
//
// /**
//   * Rather than "Type all the things", this generator restricts itself to typing all the indexes.
//   */
// object QuillCodeGen {
//
//   def index[K, V](kvs: Traversable[(K, V)]): Map[K, Traversable[V]] =
//     kvs groupBy (_._1) map {
//       case (k, kvs) => (k, kvs map (_._2))
//     }
//
//   type Error    = String
//   type Or[R, L] = Either[L, R] // SWIDT?
//   type Legit[A] = A Or Error // kawaii, ne?
//
//   val Error = Left
//   val Legit = Right
//
//   val defaultTypeMap = Map(
//     // "money"       -> "BigDecimal", // no. Cast out because of entanglment with locale (lc_stuff)
//     "int2"        -> "Short",
//     "int4"        -> "Int",
//     "int8"        -> "Long",
//     "serial"      -> "Int",
//     "serial4"     -> "Int",
//     "bigserial"   -> "Long",
//     "serial8"     -> "Long",
//     "float8"      -> "Double",
//     "numeric"     -> "BigDecimal",
//     "decimal"     -> "BigDecimal",
//     "varchar"     -> "String",
//     "text"        -> "String",
//     "bool"        -> "Boolean",
//     "bytea"       -> "Array[Byte]", // PostgreSQL
//     "uuid"        -> "java.util.UUID", // H2, PostgreSQL
//     "timestamp"   -> "LocalDateTime",
//     "timestamptz" -> "OffsetDateTime",
//     "interval"    -> "Duration",
//     "tstzrange"   -> "PgRange[OffsetDateTime]",
//     "json"        -> "Json",
//     "jsonb"       -> "Json"
//   )
//
//   def apply(
//       log: Logger,
//       driver: String,
//       url: String,
//       user: String,
//       password: String,
//       pkg: String,
//       imports: Seq[String],
//       file: File,
//       schema: String = "public",
//       typeMap: Map[String, String] = defaultTypeMap,
//       excludedTables: Set[String] = Set("schema_version"),
//       rns: ReverseNamingStrategy = ReverseEscapingSnakeCase
//   ): Unit = {
//
//     val startTime      = System.currentTimeMillis()
//     val _jdbc          = (Class forName driver).newInstance()
//     val db: Connection = DriverManager getConnection (url, user, password)
//
//     // table type: Typical types are
//     //  "TABLE", "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY", "LOCAL TEMPORARY", "ALIAS", "SYNONYM".
//     // tableType = row getString "TABLE_TYPE"
//     /** */
//     case class Table private (
//         name: String,
//         columns: Seq[Column],
//         tableType: String = ""
//     ) {
//       // TODO foreign keys? Indexes???
//
//       def toCode: String = {
//
//         val scalaName = rns table name
//
//         val applyArgs = for (col <- columns)
//           yield s"${col.asValueName}: ${col.scalaOptionType}"
//
//         val valueClasses = columns filter (c => c.isPk && c.reference.isEmpty) map (_.asValueClass)
//
//         s"""|/**
//             |  * table $schema.$name
//             |  */
//             |case class $scalaName(
//             |  ${applyArgs mkString ",\n  "}
//             |)
//             |object $scalaName {
//             |  ${valueClasses mkString "\n  "}
//             |}""".stripMargin
//       }
//     }
//     object Table {
//
//       def fkRows(tableName: String) =
//         results(db.getMetaData getExportedKeys (null, schema, tableName))
//
//       def apply(tableName: String): Table = {
//
//         def pkRows  = results(db.getMetaData getPrimaryKeys (null, null, tableName))
//         val pkNames = (pkRows map (_ getString "COLUMN_NAME")).toSet
//
//         def columnRows = results(db.getMetaData getColumns (null, schema, tableName, null))
//
//         val columns: Seq[Legit[Column]] =
//           (for {
//             row <- columnRows
//           } yield {
//             Column parse (row, tableName, Tables.foreignKeys, pkNames)
//           }).toSeq
//
//         columns collect {
//           case Error(colName) =>
//             log warn s"UNMAPPED: table: $tableName col $colName"
//         }
//         new Table(tableName, columns collect { case Legit(col) => col })
//       }
//     }
//
//     /** */
//     case class ForeignKey(from: ColumnFullName, to: ColumnFullName)
//     object ForeignKey {
//       def apply(pair: (ColumnFullName, ColumnFullName)): ForeignKey = pair match {
//         case (from, to) => ForeignKey(from, to)
//       }
//     }
//
//     /** */
//     case class ColumnFullName(tableName: String, columnName: String) {
//       def asValueType = s"${rns table tableName}.${rns default columnName}"
//     }
//
//     /** */
//     case class ColumnType(jdbcType: String,
//                           pgType: String,
//                           scalaType: String,
//                           columnSize: Int,
//                           decimalDigits: Int,
//                           columnDef: Option[String])
//     object ColumnType {
//
//       // Data source dependent type name, for a UDT the type name is fully qualified
//       private def pgType(row: ResultSet) = (row getString "TYPE_NAME").toLowerCase
//
//       def parse(row: ResultSet): Legit[ColumnType] = parse(row, pgType(row))
//
//       val EnumNamingConvention = """(.*)_e""".r
//       // val ArrayNamingConvention = """(.*)\[\]""".r
//       object ArrayNamingConvention {
//         def unapply(s: String): Option[String] = if (s startsWith "_") Some(s drop 1) else None
//       }
//
//       private def parse(row: ResultSet, pgType: String): Legit[ColumnType] = {
//
//         def scalaTypeFor(pgType: String): Legit[String] = pgType match {
//
//           case ArrayNamingConvention(scalar) =>
//             log debug s"Array: pgType=$pgType; scalar=$scalar"
//             scalaTypeFor(scalar)
//               .fold(
//                 scalarError => Error(s"Unmapped: array $pgType: no mapped scalar: $scalarError"),
//                 scalarScalaType => Legit(s"Seq[$scalarScalaType]")
//               )
//
//           case EnumNamingConvention(root) =>
//             log debug s"Enum: pgType=$pgType; root=$root"
//             val scalaType = rns default pgType
//             Either cond [Error, String] (
//               Tables.enumMap contains scalaType,
//               scalaType,
//               s"Unmapped: enum?! pgType=$pgType; root=$root"
//             )
//
//           case _ =>
//             (typeMap get pgType).fold(
//               Error(s"Unmapped: $pgType"): Legit[String] // type annotation necessary
//             ) { scalaType =>
//               Legit(scalaType)
//             }
//         }
//
//         scalaTypeFor(pgType) map { scalaType =>
//           ColumnType(
//             jdbcType = jdbcTypeToString(row getInt "DATA_TYPE"),
//             pgType = pgType,
//             scalaType = scalaType,
//             columnSize = row getInt "COLUMN_SIZE", // for varchar?
//             decimalDigits = row getInt "DECIMAL_DIGITS", // zero if not decimal
//             columnDef = Option(row getString "COLUMN_DEF") // NULL => null => None
//           )
//         }
//       }
//     }
//
//     /** */
//     case class Column(tableName: String,
//                       columnName: String,
//                       ordinal: Int, // index; starts at 1
//                       isPk: Boolean,
//                       autoIncrement: Boolean,
//                       nullable: Boolean,
//                       columnType: ColumnType,
//                       reference: Option[ColumnFullName] = None,
//                       remarks: String = "") {
//
//       def scalaOptionType: String =
//         (reference, columnType.scalaType, isPk) match {
//           case (Some(ref), _, _)             => ref.asValueType
//           case (_, _, true)                  => s"${rns table tableName}.$asTypeName"
//           case (_, scalaType, _) if nullable => s"Option[$scalaType]"
//           case (_, scalaType, _)             => scalaType
//         }
//
//       lazy val asValueName: String = rns column columnName
//       lazy val asTypeName: String  = rns table columnName
//
//       def asValueClass: String =
//         s"case class $asTypeName(value: ${columnType.scalaType}) extends AnyVal"
//     }
//     object Column {
//       def parse(row: ResultSet, tableName: String, foreignKeys: Seq[ForeignKey], pkNames: Set[String]): Legit[Column] =
//         (ColumnType parse row) map { ct =>
//           val columnName = row getString "COLUMN_NAME"
//           new Column(
//             tableName = tableName,
//             columnName = columnName,
//             columnType = ct,
//             isPk = pkNames contains columnName,
//             ordinal = row getInt "ORDINAL_POSITION",
//             nullable = row getBoolean "NULLABLE",
//             autoIncrement = row getBoolean "IS_AUTOINCREMENT",
//             // house policy for `remarks`: NULL deemed to be empty
//             remarks = Option(row getString "REMARKS").fold("")(identity),
//             reference = foreignKeys
//               .find { (fk: ForeignKey) =>
//                 fk.from == ColumnFullName(tableName, columnName)
//               }
//               .map(_.to)
//           )
//         }
//     }
//     object Tables {
//
//       def tableRows: Traversable[(ResultSet, String)] = {
//
//         def tableMetaData = db.getMetaData getTables (null, schema, "%", Array("TABLE"))
//
//         def unsafeTableName(row: ResultSet) = row getString "TABLE_NAME" // can be null
//
//         for {
//           row <- results(tableMetaData) if unsafeTableName(row) != null
//           name = unsafeTableName(row) // which we know isn't null
//         } yield (row, name)
//       }
//
//       lazy val tables: Seq[Table] =
//         (for {
//           (_, tableName) <- tableRows
//         } yield Table(tableName)).toSeq
//
//       lazy val foreignKeys: Seq[ForeignKey] = {
//
//         val FK_TABLE_NAME  = "fktable_name"
//         val FK_COLUMN_NAME = "fkcolumn_name"
//         val PK_TABLE_NAME  = "pktable_name"
//         val PK_COLUMN_NAME = "pkcolumn_name"
//
//         // construct raw `ForeignKey`s from the metadata `ResultSet`
//         val unresolvedFKs =
//           for {
//             (_, tableName) <- tableRows
//             row            <- Table fkRows tableName
//           } yield
//             ForeignKey(
//               from = ColumnFullName(row getString FK_TABLE_NAME, row getString FK_COLUMN_NAME),
//               to = ColumnFullName(row getString PK_TABLE_NAME, row getString PK_COLUMN_NAME)
//             )
//
//         // "resolve" by following references
//         val fks = for {
//           xfk <- unresolvedFKs
//           yfk <- unresolvedFKs
//         } yield // two hops reduced to one...
//         if (xfk.to == yfk.from) {
//           log info s"remapped foreign key chaining at ${xfk.to}"
//           ForeignKey(from = xfk.from, to = yfk.to)
//         } else {
//           xfk
//         }
//         fks.toSeq // really shouldn't remove duplicates...
//       }
//
//       lazy val enumMap: String Map Traversable[String] = {
//
//         val enumSql =
//           """|SELECT t.typname, e.enumlabel
//              |FROM pg_type t JOIN pg_enum e ON t.orderKey = e.enumtypid;
//              |""".stripMargin
//
//         val enumRs = db.createStatement executeQuery enumSql
//
//         val mkEnumValPair: ResultSet => (String, String) =
//           row => (row getString "typname", row getString "enumlabel")
//
//         index(
//           results(enumRs) map mkEnumValPair
//         ) map {
//           case (key, values) =>
//             (rns table key, values map (rns default _))
//         }
//       }
//
//       def orderingMixin: String = "CatsOrder"
//
//       def e8mCode =
//         for ((k, vs) <- enumMap)
//           yield {
//             val es = vs map (e => s"case object $e extends $k")
//             s"""|sealed trait ${k} extends EnumEntry
//                 |
//                 |object $k extends Enum[$k] with $orderingMixin[$k] {
//                 |
//                 |  ${es mkString "\n  "}
//                 |
//                 |  val values = findValues
//                 |
//                 |}""".stripMargin
//           }
//
//       def tab(xs: String) = xs.lines map (l => s"  $l") mkString "\n"
//
//       def code =
//         s"""|package ${pkg}
//             |
//             |${imports map (p => s"import $p") mkString "\n"}
//             |
//             |/**
//             |  * Generated code - do not modify
//             |  * QuillCodeGen
//             |  * ${java.time.ZonedDateTime.now}
//             |  */
//             |object Tables {
//             |
//             |${tab(Tables.tables map (_.toCode) mkString "\n\n")}
//             |}
//             |
//             |${e8mCode.toSeq mkString "\n\n"}
//          """.stripMargin
//
//     }
//
//     val path = file.toPath
//     log info s"Starting output generation for $path..."
//
//     if (path.getParent.toFile.mkdirs) log info s"qcg: created dirs for $path"
//
//     Files write (path, Tables.code.getBytes)
//     db.close()
//
//     log info Tables.tables.toString
//     log success s"Done! Wrote to ${file.toURI} (${System.currentTimeMillis() - startTime}ms)"
//   }
//
//   // this only works because it's ~lazy~ non-strict. like me.
//   def results(rs: ResultSet): Stream[ResultSet] =
//     if (rs.next())
//       new Iterator[ResultSet] {
//         private[this] var first = true; // heh the semicolon is a subconscious tic - I'll keep it...
//         def hasNext             = first || !rs.isLast()
//         def next() = {
//           if (first) first = false else rs.next()
//           rs
//         }
//       }.toStream
//     else Stream.empty
// }
// // ---
//
// trait ReverseNamingStrategy {
//
//   private[this] def maybeDepluralize(s: String): String =
//     s.depluralizeOption.fold(s)(identity)
//
//   final def default(s: String): String = CamelCaser rehump s
//   final def table(s: String): String   = maybeDepluralize(default(s))
//   def column(s: String): String
//
// }
//
// object ReverseSnakeCase extends ReverseNamingStrategy {
//
//   override def column(s: String): String = CamelCaser decap default(s)
// }
//
// object ReverseEscapingSnakeCase extends ReverseNamingStrategy {
//
//   import CamelCaser.{ decap, escape }
//
//   override def column(s: String): String = escape(decap(default(s)))
// }
//
// object CamelCaser {
//
//   def rehump(s: String): String =
//     (s.toLowerCase split "_" map capitalize).mkString
//   def decap(s: String): String  = s.head.toLower +: s.tail
//   def escape(s: String): String = if (reservedWords contains s) s"`$s`" else s
//
//   private def capitalize(s: String): String = s match {
//     case "" => "_"
//     case s  => s.capitalize
//   }
//
//   private val reservedWords = Set(
//     "abstract",
//     "case",
//     "catch",
//     "class",
//     "def",
//     "do",
//     "else",
//     "extends",
//     "false",
//     "final",
//     "finally",
//     "for",
//     "forSome",
//     "if",
//     "implicit",
//     "import",
//     "lazy",
//     "match",
//     "new",
//     "null",
//     "object",
//     "override",
//     "package",
//     "private",
//     "protected",
//     "return",
//     "sealed",
//     "super",
//     "this",
//     "throw",
//     "trait",
//     "try",
//     "true",
//     "type",
//     "val",
//     "var",
//     "while",
//     "with",
//     "yield",
//   )
//
// }
// object DepluralizerImplicit {
//
//   object StringDepluralizer {
//
//     val RuleId  = """(\w+)(?i:sheep|series|is|us)""".r
//     val RuleXes = """(\w+)(x|ss|z|ch|sh)es""".r
//     val RuleIes = """(\w+)ies""".r
//     val RuleEn  = """(\w+x)en""".r
//     val RuleS   = """(\w+)s""".r
//   }
//
//   implicit class StringDepluralizer(val s: String) extends AnyVal {
//     import StringDepluralizer._
//
//     def depluralize = s match {
//       case "pizzas"        => "Pizza"   // exceptional Pizza
//       case RuleId(t)       => t         // t is its own plural
//       case RuleXes(t, end) => s"$t$end" // maybe lops off the final "es"
//       case RuleIes(t)      => s"${t}y"  // parties => party
//       case RuleEn(t)       => t         // oxen => ox
//       case RuleS(t)        => t         // cars => car
//       case _ =>
//         throw new IllegalArgumentException(
//           s"sorry - we seem to need a new depluralize() rule for $s"
//         )
//     }
//     def depluralizeOption = scala.util.Try(depluralize).toOption
//   }
// }
//
// object XkConst {
//
//   type XkConst = String // Refined XkTag
//
//   // getExportedKeys
//   val PKTABLE_CAT: XkConst   = "PKTABLE_CAT"   // primary key table catalog (may be null)
//   val PKTABLE_SCHEM: XkConst = "PKTABLE_SCHEM" // primary key table schema (may be null)
//   val PKTABLE_NAME: XkConst  = "PKTABLE_NAME"  // primary key table name
//   val PKCOLUMN_NAME: XkConst = "PKCOLUMN_NAME" // primary key column name
//   val FKTABLE_CAT: XkConst   = "FKTABLE_CAT"   // foreign key table catalog (may be null) being exported (may be null)
//   val FKTABLE_SCHEM: XkConst = "FKTABLE_SCHEM" // foreign key table schema (may be null) being exported (may be null)
//   val FKTABLE_NAME: XkConst  = "FKTABLE_NAME"  // foreign key table name being exported
//   val FKCOLUMN_NAME: XkConst = "FKCOLUMN_NAME" // foreign key column name being exported
//   val KEY_SEQ: XkConst       = "KEY_SEQ"       // sequence number within foreign key( a value of 1 represents the first column of the foreign key, a value of 2 would represent the second column within the foreign key).
//   val UPDATE_RULE: XkConst   = "UPDATE_RULE"   // What happens to foreign key when primary is updated:
//   // importedNoAction - do not allow update of primary key if it has been imported
//   // importedKeyCascade - change imported key to agree with primary key update
//   // importedKeySetNull - change imported key to NULL if its primary key has been updated
//   // importedKeySetDefault - change imported key to default values if its primary key has been updated
//   // importedKeyRestrict - same as importedKeyNoAction (for ODBC 2.x compatibility)
//   val DELETE_RULE: XkConst = "DELETE_RULE" // What happens to the foreign key when primary is deleted.
// // importedKeyNoAction - do not allow delete of primary key if it has been imported
// // importedKeyCascade - delete rows that import a deleted key
// // importedKeySetNull - change imported key to NULL if its primary key has been deleted
// // importedKeyRestrict - same as importedKeyNoAction (for ODBC 2.x compatibility)
// // importedKeySetDefault - change imported key to default if its primary key has been deleted
//   val FK_NAME: XkConst       = "FK_NAME"       // foreign key name (may be null)
//   val PK_NAME: XkConst       = "PK_NAME"       // primary key name (may be null)
//   val DEFERRABILITY: XkConst = "DEFERRABILITY" // can the evaluation of foreign key constraints be deferred until commit
// // importedKeyInitiallyDeferred - see SQL92 for definition
// // importedKeyInitiallyImmediate - see SQL92 for definition
// // importedKeyNotDeferrable - see SQL92 for definition
//
// }
// object PkConst {
//
//   type PkConst = String // Refined PkTag
//   // val PK_NAME        = "pk_name"
//
//   // getPrimaryKeys()
//   val TABLE_CAT: PkConst   = "TABLE_CAT"   // table catalog (may be null)
//   val TABLE_SCHEM: PkConst = "TABLE_SCHEM" // table schema (may be null)
//   val COLUMN_NAME: PkConst = "COLUMN_NAME" // column name
//   val KEY_SEQ: PkConst     = "KEY_SEQ"     // sequence number within primary key( a value of 1 represents the first column of the primary key, a value of 2 would represent the second column within the primary key).
//   val PK_NAME: PkConst     = "PK_NAME"     // primary key name (may be null)
//
// }
//
// object jdbcTypeToString {
//
//   import java.sql.Types._
//
//   def apply(jdbcType: Int) = jdbcType match {
//     case ARRAY                   => s"jdbc:array" // 2003
//     case BIGINT                  => s"jdbc:bigint" // -5
//     case BINARY                  => s"jdbc:binary" // -2
//     case BIT                     => s"jdbc:bit" // -7
//     case BLOB                    => s"jdbc:blob" // 2004
//     case BOOLEAN                 => s"jdbc:boolean" // 16
//     case CHAR                    => s"jdbc:char" // 1
//     case CLOB                    => s"jdbc:clob" // 2005
//     case DATALINK                => s"jdbc:datalink" // 70
//     case DATE                    => s"jdbc:date" // 91
//     case DECIMAL                 => s"jdbc:decimal" // 3
//     case DISTINCT                => s"jdbc:distinct" // 2001
//     case DOUBLE                  => s"jdbc:double" // 8
//     case FLOAT                   => s"jdbc:float" // 6
//     case INTEGER                 => s"jdbc:integer" // 4
//     case JAVA_OBJECT             => s"jdbc:java_object" // 2000
//     case LONGNVARCHAR            => s"jdbc:longnvarchar" // -16
//     case LONGVARBINARY           => s"jdbc:longvarbinary" // -4
//     case LONGVARCHAR             => s"jdbc:longvarchar" // -1
//     case NCHAR                   => s"jdbc:nchar" // -15
//     case NCLOB                   => s"jdbc:nclob" // 2011
//     case NULL                    => s"jdbc:null" // 0
//     case NUMERIC                 => s"jdbc:numeric" // 2
//     case NVARCHAR                => s"jdbc:nvarchar" // -9
//     case OTHER                   => s"jdbc:other" // 1111
//     case REAL                    => s"jdbc:real" // 7
//     case REF                     => s"jdbc:ref" // 2006
//     case REF_CURSOR              => s"jdbc:ref_cursor" // 2012
//     case ROWID                   => s"jdbc:rowid" // -8
//     case SMALLINT                => s"jdbc:smallint" // 5
//     case SQLXML                  => s"jdbc:sqlxml" // 2009
//     case STRUCT                  => s"jdbc:struct" // 2002
//     case TIME                    => s"jdbc:time" // 92
//     case TIME_WITH_TIMEZONE      => s"jdbc:time_with_timezone" // 2013
//     case TIMESTAMP               => s"jdbc:timestamp" // 93
//     case TIMESTAMP_WITH_TIMEZONE => s"jdbc:timestamp_with_timezone" // 2014
//     case TINYINT                 => s"jdbc:tinyint" // -6
//     case VARBINARY               => s"jdbc:varbinary" // -3
//     case VARCHAR                 => s"jdbc:varchar" // 12
//   }
// }
