package io.deftrade.sbt

import java.io.File 
/**
 *  This customizes the Slick code generator. We only do simple name mappings.
 *  For a more advanced example see https://github.com/cvogt/slick-presentation/tree/scala-exchange-2013
 */
object QuillCodeGen {

  type EnumModel = Vector[(String, String)] // enum -> value

  def apply(driver: String, url: String, user: String, password: String,
    folder: File, // output directory for generated source
    pkg: String): Unit = ???
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
