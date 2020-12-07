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

import java.nio.file.{ Paths }

trait dsl {

  /**
    */
  def valueStore[F[_]: Sync: ContextShift](param: ValueStore.Param) = VsOps[F](param)

  /**
    */
  def keyValueStore[F[_]: Sync: ContextShift](param: KeyValueStore.Param) = KvsOps[F](param)
}

/** dsl for value stores
  */
final case class VsOps[F[_]: Sync: ContextShift](final val param: ValueStore.Param) {

  /** `at` clause
    */
  def at(p: String) = AddressOps(p)

  /**
    */
  sealed case class AddressOps(p: String) {

    private def path = Paths get p

    /**
      */
    def ofContentAddressed[V: Eq: Show, K2: Order: Show, V2: Eq: Show, HV <: HList](
        v: WithId.Aux[V]
    )(
        thunk: param.DependentTypeThunk[V]
    )(
        subThunk: thunk.SubThunk[K2, V2]
    )(implicit
      // IsV: p.ValueSpec[K2, V2] === v.Value,
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[subThunk.ValueStore[F]] =
      Result safe {
        new CaMfValueStore[F, v.Value, HV](
          v,
          path
        ) with subThunk.ValueStore[F] {}
      }

    /** `of` clause
      */
    def ofChainAddressed[V: Eq: Show, K2: Order: Show, V2: Eq: Show, HV <: HList](
        v: WithId.Aux[V]
    )(
        thunk: param.DependentTypeThunk[V]
    )(
        subThunk: thunk.SubThunk[K2, V2]
    )(implicit
      // IsV: p.ValueSpec[K2, V2] === v.Value,
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]): Result[subThunk.ValueStore[F]] =
      Result safe {
        new ChMfValueStore[F, V, HV](v, path) with subThunk.ValueStore[F] {}
      }
  }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
final case class KvsOps[F[_]: Sync: ContextShift](param: KeyValueStore.Param) {

  /**
    */
  def at(p: String) = AddressOps(p)

  /**
    */
  sealed case class AddressOps(p: String) {

    def ofKeyChained[K: Show, V: Eq: Show, K2: Order: Show, V2: Eq: Show, HV <: HList](
        subThunk: param.DependentTypeThunk[K, V]#SubThunk[K2, V2]
    )(implicit
      // isV: param.ValueSpec[K2, V2] === V,
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]],
      lgetk: Lazy[Get[K]],
      lputk: Lazy[Put[K]]) =
      Result safe {

        implicit val kGet = lgetk.value
        implicit val kPut = lputk.value

        new MemFileKeyValueStore(subThunk.kv, Paths get p) with subThunk.KeyValueStore[F] {

          import V._
          final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
        }
      }

    // K2: Order: Show, V2: Eq: Show

    /**
      */
    def ofChainAddressed[K: Show, V: Eq: Show, K2: Order: Show, V2: Eq: Show, HV <: HList](
        kv: WithKey.Aux[K, V]
    )(
        thunk: param.DependentTypeThunk[K, V]
    )(
        subThunk: thunk.SubThunk[K2, V2]
    )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]],
      lgetk: Lazy[Get[K]],
      lputk: Lazy[Put[K]]): Result[subThunk.KeyValueStore[F]] =
      Result safe {

        implicit val kGet = lgetk.value
        implicit val kPut = lputk.value

        new MemFileKeyValueStore(kv, Paths get p) with subThunk.KeyValueStore[F] {

          import V._
          final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
        }
      }
  }
}
