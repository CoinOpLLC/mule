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

trait dsl {

  /**
    */
  def valueStore[F[_]: Sync: ContextShift] = VsOps[F]()

  /**
    */
  def keyValueStore[F[_]: Sync: ContextShift] = KvsOps[F]()
}

/** dsl for value stores
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
        K2: Order: Show,
        V2: Eq: Show,
        HV <: HList
    ](
        V: ValueStores[V]
    )(implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[V.ValueStore[F]] =
      Result safe {

        import V.{ Row, Value }

        new CsvValueStore[F, Value](V) with V.ValueStore[F] with MemFileV[F, V] {

          def path: Path = Paths get p

          final override lazy val fresh = Fresh.shaContent[Row]

          final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderV
          final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderV
        }
      }

    /** `of` clause
      */
    def ofChainAddressed[
        V: Eq: Show,
        K2: Order: Show,
        V2: Eq: Show,
        HV <: HList
    ](
        V: ValueStores[V]
    )(implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[V.ValueStore[F]] =
      Result safe {

        import V.{ Row, Value }

        new CsvValueStore[F, Value](V) with V.ValueStore[F] with MemFileV[F, V] {

          def path: Path = Paths get p

          final override lazy val fresh = Fresh.shaChain[Row]

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
        K: Show: Get: Put,
        V: Eq: Show,
        K2: Order: Show,
        V2: Eq: Show,
        HV <: HList
    ](
        KV: KeyValueStores[K, V]
    )(implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[KV.KeyValueStore[F]] =
      Result safe {

        import KV.{ Id, Key, Row, Value }

        new CsvKeyValueStore(KV) with KV.KeyValueStore[F] with MemFileKV[F, Key, Value] {

          def path: Path = Paths get p

          final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]

          final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderKv
          final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderKv
        }
      }
  }
}
