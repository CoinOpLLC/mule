package io.deftrade
package keyval

import impl._

import cats.Eq
import cats.effect.{ ContextShift, Sync }

import shapeless.{ HList, LabelledGeneric, Lazy }
// import shapeless.labelled._

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import fs2.{ Pipe }

import java.nio.file.{ Paths }

/** dsl for value stores: `at` clause */
final case class VsOps[F[_]: Sync: ContextShift]() {

  /** */
  def at(p: String) = AddressOps(p)

  /** dsl for value stores: `of` clause */
  sealed case class AddressOps(p: String) {

    /** */
    def ofChainAddressed[V: Eq, HV <: HList](
        v: WithId[V],
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[MemFileValueStore[F, V, HV]] = Result safe {
      new MemFileValueStore[F, V, HV](v) {

        import V._

        /** */
        final override def path = Paths get p

        /** */
        final lazy val idRowToCSV: Pipe[Effect, (Id, Row), String] = deriveCsvEncoderV

        /** */
        final lazy val csvToIdRow: Pipe[Effect, String, Result[(Id, Row)]] = deriveCsvDecoderV

        /** */
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }

    /** */
    def ofContentAddressed[V: Eq, HV <: HList](
        v: WithId[V],
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[MemFileValueStore[F, V, HV]] = Result safe {
      new MemFileValueStore[F, V, HV](v) {

        import V._

        /** */
        final override def path = Paths get p

        /** */
        final lazy val idRowToCSV: Pipe[Effect, (Id, Row), String] = deriveCsvEncoderV

        /** */
        final lazy val csvToIdRow: Pipe[Effect, String, Result[(Id, Row)]] = deriveCsvDecoderV

        /** FIXME implementation is wrong */
        // final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
        final protected lazy val fresh: Fresh[Id, Row] = ??? // Fresh.shaContentAddress[Row]
      }
    }
  }
}

/** dsl for key value stores: `of` clause */
final case class KvsOps[F[_]: Sync: ContextShift]() {

  /** */
  def at(p: String) = AddressOps(p)

  /** dsl for key value stores: `of` clause */
  sealed case class AddressOps(p: String) {

    /** */
    def ofChainAddressed[K, V: Eq, HV <: HList](
        kv: WithKey.Aux[K, V]
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]],
        lgetk: Lazy[Get[K]],
        lputk: Lazy[Put[K]]
    ): Result[MemFileKeyValueStore[F, K, V, HV]] = Result safe {
      new MemFileKeyValueStore[F, K, V, HV](kv) { self =>

        import V._

        /** */
        final override def path = Paths get p

        /** */
        final lazy val idRowToCSV: Pipe[Effect, (Id, Row), String] = deriveCsvEncoderKv

        /** */
        final lazy val csvToIdRow: Pipe[Effect, String, Result[(Id, Row)]] = deriveCsvDecoderKv

        /** */
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }
  }

  /** */
  def of(V: WithKey) = TypeOps(V)

  /** */
  sealed case class TypeOps(final val V: WithKey) {
    final type StoreType = KeyValueStore[F, V.Key, V.Value]
  }
}
