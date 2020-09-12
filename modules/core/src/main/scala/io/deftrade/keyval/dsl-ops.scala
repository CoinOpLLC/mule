package io.deftrade
package keyval

import impl._

import cats.implicits._
import cats.{ Eq, Show }
import cats.effect.{ ContextShift, Sync }
import cats.evidence._

import shapeless.{ HList, LabelledGeneric, Lazy }
// import shapeless.labelled._

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import java.nio.file.{ Paths }

trait KVSspec
object KVSspec {

  // V:=Value; L:=List; M:=Map; R:=Replace; S:=Sum; C:=Cat;
  case object VRLC  extends KVSspec
  case object VSLC  extends KVSspec
  case object LVRC  extends KVSspec
  case object MVRLC extends KVSspec
  case object MVSLC extends KVSspec
  case object MLVRC extends KVSspec
}

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

    private def path = Paths get p

    /**
      */
    def ofContentAddressed[V: Eq: Show, HV <: HList, K2: Show, V2: Show](
        v: WithId.Aux[V]
    )(
        vs: v.VsSpec.Aux[K2, V2]
    )(implicit
        lgv: LabelledGeneric.Aux[V, HV], // use `wut` to coerce `lgv` to the right type
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[ValueStore[F, V]] =
      Result safe {
        import VsParam._
        vs.param match {
          case V =>
            new CaMfValueStore[F, V, HV](v, path) with ValueStoreV[F, V] {}
          case LV => ???
          // new CaMfValueStore[F, Option[V2], HV](v, path) with ValueStoreLV[F, V2] {}
          // case VsSpec.MK2V2    => new CaMfValueStore[F, V, HV](v, path) with ValueStoreMK2V2[F, V] {}
          // case VsSpec.MK2LV2   => new CaMfValueStore[F, V, HV](v, path) with ValueStoreMK2LV2[F, V] {}
          // case VsSpec.NELV     => new CaMfValueStore[F, V, HV](v, path) with ValueStoreNELV[F, V] {}
          // case VsSpec.NEMK2V2  => new CaMfValueStore[F, V, HV](v, path) with ValueStoreNEMK2V2[F, V] {}
          // case VsSpec.NEMK2LV2 => new CaMfValueStore[F, V, HV](v, path) with ValueStoreNEMK2LV2[F, V] {}
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
    ): Result[ValueStore[F, V]] =
      Result safe {

        new ChMfValueStore[F, V, HV](v, path) with ValueStoreV[F, V] {}
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

          /** Key Value stores ''must'' use chained addresses.
            *
            * (This is down to semantics, not crypto.)
            */
          final type Shape[A] = Map[V.Key, A]

          final protected lazy val fresh: Fresh[V.Id, V.Row] = Fresh.shaChain[V.Row]
        }
      }
  }
}
