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

import java.nio.file.{ Paths }

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
    def ofContentAddressed[V: Eq: Show, HV <: HList](
        v: WithId.Aux[V]
    )(implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[MemFileValueStore[F, V, HV]] =
      Result safe {

        new MemFileValueStore[F, V, HV](v, Paths get p) {

          final type Shape[x] = Set[x]

          final protected lazy val fresh: Fresh[V.Id, V.Row] = Fresh.shaContent[V.Row]
        }
      }

    /** `of` clause
      */
    def ofChainAddressed[V: Eq: Show, HV <: HList](
        v: WithId.Aux[V]
    )(implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[MemFileValueStore[F, V, HV]] =
      Result safe {

        new MemFileValueStore[F, V, HV](v, Paths get p) {

          final type Shape[x] = List[x]

          final protected lazy val fresh: Fresh[V.Id, V.Row] = Fresh.shaChain[V.Row]
        }
      }
  }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
final case class KvsOps[F[_]: Sync: ContextShift]() {

  /**
    */
  def at(p: String) = AddressOps(p)

  /**
    */
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
        lputk: Lazy[Put[K]]
    ): Result[MemFileKeyValueStore[F, K, V, HV]] =
      Result safe {

        implicit val kGet = lgetk.value
        implicit val kPut = lputk.value

        new MemFileKeyValueStore(kv, Paths get p) {

          final protected lazy val fresh: Fresh[V.Id, V.Row] = Fresh.shaChain[V.Row]
        }
      }
  }
}
