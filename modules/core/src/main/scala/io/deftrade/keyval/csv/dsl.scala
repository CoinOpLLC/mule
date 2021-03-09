package io.deftrade
package keyval
package csv

import cats.implicits._
import cats.{ Eq, Order, Show }
import cats.effect.{ ContextShift, Sync }

import shapeless.{ HList, LabelledGeneric, Lazy }

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import fs2.Pipe

import java.nio.file.{ Path, Paths }

trait dsl {

  /**
    */
  def csvVS[F[_]: Sync: ContextShift] = VsOps[F]()

  /**
    */
  def csvKVS[F[_]: Sync: ContextShift] = KvsOps[F]()
}

/** DSL for value stores
  */
final case class VsOps[F[_]: Sync: ContextShift]() { ops =>

  val (x, y) = (ContextShift[F], Sync[F])

  /** `at` clause
    */
  def at(p: String) = AddressOps(p)

  /**
    */
  sealed case class AddressOps(p: String) {

    /**
      */
    def ofContentAddressed[
        V: Eq: Show,
        HV <: HList
    ](
        VS: ValueStores[V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[VS.ValueStore[F]] =
      Result safe {

        new CsvValueStore[F, V](VS) with VS.ValueStore[F] with MemFileV[F, V] {

          implicit val X = x
          implicit val F = y

          def path: Path =
            Paths get s"""${p}/${VS.productPrefix}"""

          final override lazy val fresh =
            NextId.shaContent[VS.Row]

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
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[VS.ValueStore[F]] =
      Result safe {

        new CsvValueStore[F, V](VS) with VS.ValueStore[F] with MemFileV[F, V] {

          implicit val X = x
          implicit val F = y

          def path: Path =
            Paths get s"""${p}/${VS.productPrefix}"""

          final override lazy val fresh =
            NextId.shaChain[VS.Row]

          final lazy val recordToCSV: Pipe[F, Record, String]         = deriveCsvEncoderV
          final lazy val csvToRecord: Pipe[F, String, Result[Record]] = deriveCsvDecoderV
        }
      }
  }
}

/**
  */
final case class KvsOps[F[_]: Sync: ContextShift]() { effect =>

  val (x, y) = (ContextShift[F], Sync[F])

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
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[KVS.KeyValueStore[F]] =
      Result safe {

        new CsvKeyValueStore[F, K, V](KVS) with KVS.KeyValueStore[F] with MemFileKV[F, K, V] {

          implicit val X = x
          implicit val F = y

          def path: Path =
            Paths get s"""${p}/${KVS.productPrefix}"""

          final protected lazy val fresh: NextId[KVS.Id, KVS.Row] =
            NextId.shaChain[KVS.Row]

          final lazy val recordToCSV: Pipe[F, Record, String]         = deriveCsvEncoderKv
          final lazy val csvToRecord: Pipe[F, String, Result[Record]] = deriveCsvDecoderKv
        }
      }
  }
}