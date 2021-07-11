package io.deftrade
package keyval
package csv

import cats.implicits._
import cats.{ Eq, Order, Show }
import cats.effect.{ Sync }

import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy }

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import fs2.Pipe

import java.nio.file.{ Path, Paths }

sealed trait Persistence
object Persistence {
  case object CSV      extends Persistence
  case object Postgres extends Persistence
}

trait dsl {

  /**
    */
  def vs[F[_]: Sync] = VsOps[F](Persistence.CSV: Persistence)

  /**
    */
  def kvs[F[_]: Sync] = KvsOps[F]()
}

abstract class ValueLogs[V, HV <: HList](
    final val VS: ValueStores[V]
)(implicit
  final val lgx: LabelledGeneric.Aux[V, HV]) { self =>

  def contentAddressed[F[_]: Sync]: Result[VS.ValueStore[F]]

  final def narrow: ValueLogs[V, HV] = self
}

object ValueLogs {
  def apply[V: Show, HV <: HList: LabelledRead: LabelledWrite](
      VS: ValueStores[V],
      p: String,
      persistence: Persistence
  )(implicit
    lgx: LabelledGeneric.Aux[V, HV]): ValueLogs[V, HV] =
    persistence match {
      case Persistence.CSV      => CsvValueLogs[V, HV](VS, p).narrow
      case Persistence.Postgres => ???
    }
}

final case class CsvValueLogs[V: Show, HV <: HList: LabelledRead: LabelledWrite](
    vs: ValueStores[V],
    p: String
)(implicit
  lgx: LabelledGeneric.Aux[V, HV])
    extends ValueLogs[V, HV](vs) {

  import cormorant.implicits._
  import cormorant.refined._
  import cormorant.generic.semiauto._

  final def contentAddressed[F[_]: Sync]: Result[VS.ValueStore[F]] =
    Result safe {

      new CsvValueStore[F, V](VS) with VS.ValueStore[F] with MemFileV[F, V] {

        def path: Path =
          Paths get s"""${p}/${VS.productPrefix}"""

        final override lazy val fresh =
          NextId.shaContent[Row]

        final lazy val recordToCSV: Pipe[F, Record, String]         = deriveCsvEncoderV
        final lazy val csvToRecord: Pipe[F, String, Result[Record]] = deriveCsvDecoderV
      }
    }
}

/** DSL for value stores
  */
final case class VsOps[F[_]: Sync](persistence: Persistence) { ops =>

  val y = Sync[F]

  /** `at` clause
    */
  def at(p: String) = AddressOps(p)

  /**
    */
  sealed case class AddressOps(p: String) {

    // def ofContentAddressedX[
    //     V: Eq: Show,
    //     HV <: HList
    // ](
    //     VS: ValueStores[V]
    // )(implicit
    //   lgx: LabelledGeneric.Aux[V, HV]): Result[VS.ValueStore[F]] =
    //   ValueLogs(VS, p, persistence).contentAddressed[F]
    // // z.chained

    /**
      */
    def ofContentAddressed[
        V: Eq: Show,
        HV <: HList
    ](
        VS: ValueStores[V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[VS.IdField :: HV]],
      llw: Lazy[LabelledWrite[VS.IdField :: HV]]): Result[VS.ValueStore[F]] =
      Result safe {

        new CsvValueStore[F, V](VS) with VS.ValueStore[F] with MemFileV[F, V] {

          implicit val F = y

          implicit val lr = llr.value
          implicit val lw = llw.value

          def path: Path =
            Paths get s"""${p}/${VS.productPrefix}"""

          final override lazy val fresh =
            NextId.shaContent[Row]

          final lazy val recordToCSV: Pipe[F, Record, String]         = deriveCsvEncoderV
          final lazy val csvToRecord: Pipe[F, String, Result[Record]] = deriveCsvDecoderV
        }
      }

    /** `of` clause
      */
    def ofChained[
        V: Eq: Show,
        HV <: HList
    ](
        VS: ValueStores[V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[VS.IdField :: HV]],
      llw: Lazy[LabelledWrite[VS.IdField :: HV]]): Result[VS.ValueStore[F]] =
      Result safe {

        new CsvValueStore[F, V](VS) with VS.ValueStore[F] with MemFileV[F, V] {

          implicit val F = y

          implicit val lr = llr.value
          implicit val lw = llw.value

          def path: Path =
            Paths get s"""${p}/${VS.productPrefix}"""

          final override lazy val fresh =
            NextId.shaChain[Row]

          final lazy val recordToCSV: Pipe[F, Record, String]         = deriveCsvEncoderV
          final lazy val csvToRecord: Pipe[F, String, Result[Record]] = deriveCsvDecoderV
        }
      }
  }
}

/**
  */
final case class KvsOps[F[_]: Sync]() { effect =>

  val y = Sync[F]

  /**
    */
  def at(p: String) = AddressOps(p)

  /**
    */
  sealed case class AddressOps(p: String) { address =>

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def ofKeyChained[
        K: Order: Show: Get: Put,
        V: Eq: Show,
        HV <: HList
    ](
        KVS: KeyValueStores[K, V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[KVS.IdField :: KVS.KeyField :: HV]],
      llw: Lazy[LabelledWrite[KVS.IdField :: KVS.KeyField :: HV]],
      llwe: Lazy[LabelledWrite[KVS.IdField :: KVS.KeyField :: HNil]])
      : Result[KVS.KeyValueStore[F]] =
      Result safe {

        new CsvKeyValueStore[F, K, V](KVS) with KVS.KeyValueStore[F] with MemFileKV[F, K, V] {

          implicit val F = y

          implicit val lr  = llr.value
          implicit val lw  = llw.value
          implicit val lwe = llwe.value

          def path: Path =
            Paths get s"""${p}/${KVS.productPrefix}"""

          final protected lazy val fresh: NextId[KVS.Id, Row] =
            NextId.shaChain[Row]

          final lazy val recordToCSV: Pipe[F, Record, String]         = deriveCsvEncoderKv
          final lazy val csvToRecord: Pipe[F, String, Result[Record]] = deriveCsvDecoderKv
        }
      }
  }
}
