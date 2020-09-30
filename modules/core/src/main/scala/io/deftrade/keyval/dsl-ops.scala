package io.deftrade
package keyval

import impl._

import cats.implicits._
import cats.{ Eq, Order, Show }
import cats.effect.{ ContextShift, Sync }
import cats.evidence._

import shapeless.{ HList, LabelledGeneric, Lazy }
// import shapeless.labelled._

import io.chrisdavenport.cormorant
import cormorant.{ Get, LabelledRead, LabelledWrite, Put }

import java.nio.file.{ Paths }
import scala.tools.asm.Label

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
    def ofContentAddressed[V: Eq: Show, K2: Order: Show, V2: Eq: Show, HV <: HList](
        v: WithId.Aux[V],
        p: ValueStore.Param
    )(implicit
        IsV: p.ValueSpec[K2, V2] === v.Value,
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[ValueStore[F, V]] =
      Result safe {

        val thunk = ValueStore(v, p).deriveKV[K2, V2]

        implicit val lgvs: LabelledGeneric.Aux[p.ValueSpec[K2, V2], HV] =
          IsV.flip substitute [LabelledGeneric.Aux[*, HV]] lgv

        implicit val vsShow = IsV.flip substitute [Show] Show[v.Value]

        import ValueStore.Param._

        p match {
          case V =>
            new CaMfValueStore[F, v.Value, HV](
              v,
              path
            ) with thunk.ValueStore[F] {}
          case LV =>
            new CaMfValueStore[F, v.Value, HV](
              v,
              path
            ) with thunk.ValueStore[F] {}
          case MKV    => ???
          case MKLV   => ???
          case NELV   => ???
          case NEMKV  => ???
          case NEMKLV => ???
        }
      }

    /** `of` clause
      */
    def ofChainAddressed[V: Eq: Show, K2: Order: Show, V2: Eq: Show, HV <: HList](
        v: WithId.Aux[V],
        p: ValueStore.Param
    )(implicit
        IsV: p.ValueSpec[K2, V2] === v.Value,
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[ValueStore[F, V]] =
      Result safe {

        val thunk = ValueStore(v, p).deriveKV[K2, V2]

        implicit val vsShow = IsV.flip substitute [Show] Show[v.Value]

        import ValueStore.Param._

        new ChMfValueStore[F, V, HV](v, path) with thunk.ValueStore[F] {}
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
