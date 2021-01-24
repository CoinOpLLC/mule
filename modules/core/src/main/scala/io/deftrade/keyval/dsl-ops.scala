package io.deftrade
package keyval

import impl._

import cats.implicits._
import cats.{ Eq, Order, Show }
import cats.effect.{ ContextShift, Sync }

import shapeless.{ HList, LabelledGeneric, Lazy }
// import shapeless.labelled._

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import java.nio.file.{ Path, Paths }

trait csvStoreDsl {

  /**
    */
  def valueStore[F[_]: Sync: ContextShift] = VsOps[F]()

  /**
    */
  def keyValueStore[F[_]: Sync: ContextShift] = KvsOps[F]()
}

/** csvStoreDsl for value stores
  */
final case class VsOps[F[_]: Sync: ContextShift]() {

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

          def path: Path =
            Paths get p

          final override lazy val fresh =
            Fresh.shaContent[VS.Row]

          final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderV
          final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderV
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

          def path: Path =
            Paths get p

          final override lazy val fresh =
            Fresh.shaChain[VS.Row]

          final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderV
          final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderV
        }
      }
  }
}

/**
  */
final case class KvsOps[F[_]: Sync: ContextShift]() { effect =>

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

          def path: Path =
            Paths get p

          final protected lazy val fresh: Fresh[KVS.Id, KVS.Row] =
            Fresh.shaChain[KVS.Row]

          final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderKv
          final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderKv
        }
      }
  }
}
