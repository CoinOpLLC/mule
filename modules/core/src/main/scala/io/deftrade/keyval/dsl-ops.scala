package io.deftrade
package keyval

import impl._

import cats.implicits._
import cats.{ Eq, Show }
import cats.effect.{ ContextShift, Sync }

import shapeless.{ HList, LabelledGeneric, Lazy }
// import shapeless.labelled._

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import fs2.{ Pipe }

import java.nio.file.{ Paths }

/** dsl for value stores: `at` clause */
final case class VsOps[F[_]: Sync: ContextShift]() {

  /**
    */
  def at(p: String) = AddressOps(p)

  /** dsl for value stores: `of` clause */
  sealed case class AddressOps(p: String) {

    /**
      */
    def ofChainAddressed[V: Eq: Show, HV <: HList](
        v: WithId.Aux[V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[MemFileValueStore[F, V, HV]] =
      Result safe {
        new MemFileValueStore[F, V, HV](v, Paths get p) {

          import V._

          final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
        }
      }

    /**
      */
    def ofContentAddressed[V: Eq: Show, HV <: HList](
        v: WithId.Aux[V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[MemFileValueStore[F, V, HV]] =
      Result safe {
        new MemFileValueStore[F, V, HV](v, Paths get p) {

          import V._

          final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaContent[Row]
        }
      }
  }
}

/** dsl for key value stores: `of` clause */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
final case class KvsOps[F[_]: Sync: ContextShift]() {

  /**
    */
  def at(p: String) = AddressOps(p)

  /** dsl for key value stores: `of` clause */
  sealed case class AddressOps(p: String) {

    /**
      */
    def ofChainAddressed[K: Show, V: Eq: Show, HV <: HList](
        kv: WithKey.Aux[K, V]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]],
      lgetk: Lazy[Get[K]],
      lputk: Lazy[Put[K]]): Result[MemFileKeyValueStore[F, K, V, HV]] =
      Result safe {
        implicit def kGet = lgetk.value
        implicit def kPut = lputk.value
        new MemFileKeyValueStore(kv, Paths get p) { self =>
          import V._

          /**
            */
          final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
        }
      }
  }

  /**
    */
  def of(V: WithKey) = TypeOps(V)

  /**
    */
  sealed case class TypeOps(final val V: WithKey) {
    final type StoreType = KeyValueStoreV[F, V.Key, V.Value]
  }
}
