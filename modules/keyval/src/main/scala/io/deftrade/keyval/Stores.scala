/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package keyval

import cats.implicits._
import cats.effect.{ Sync }

import shapeless.labelled.FieldType

import eu.timepit.refined
import refined.api.Refined
import refined.cats._

import fs2.{ Pipe, Stream }

/** Defines `Id` and other persistence helpers for a given value class `V`.
  */
trait Stores[V] extends Product {

  /**
    */
  final type Value = V

  /** `Id`s are primary keys, and defined to be secure hashes of some kind.
    */
  final type Id = SHA

  /** The full type of the [[Id]] column.
    */
  final type IdField = FieldType[id.T, Id]

  /** Think spreadsheet or relational table,
    */
  type Row

  /** Base class
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  trait Store[F[_]] {

    implicit val F: Sync[F]

    /**
      */
    final type Record = (Id, Row)

    /** implementations may override. TODO: Revisit this decision.
      */
    def rows: Stream[F, Row] =
      records map (_._2)

    /** implementations may override. TODO: Revisit this decision.
      */
    def hasId(id: Id): F[Boolean] =
      (records exists (_._1 === id)).compile.lastOrError

    /** Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
      */
    protected def records: Stream[F, Record]

    /**  Returns ''all'' `Row`s with the given `Id` (none, if not found) as an [[fs2.Stream]].
      *  implementations may override. TODO: Revisit this decision.
      */
    protected def rows(id: Id): F[List[Row]] =
      (records filter (_._1 === id) map (_._2)).compile.toList

    /** overrideable with default nop
      * empty List entails `del`etion (only applicable to [[KeyValueStores]])
      */
    protected def cacheFill(id: Id, rows: List[Row]): F[Unit] =
      ().pure[F]

    /**
      */
    protected def persist: Pipe[F, Record, Unit]

    /**
      */
    protected def fresh: NextId[Id, Row]

    /** note this is pure and not effectful! */
    protected def nextId(row: Row, rows: Row*): Id =
      fresh.nextAll(prev, row, rows: _*)

    /** FIXME obviously... this works, not obvously, that's the problem
      */

    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    protected var prev: Id =
      Refined unsafeApply [String, IsSHA] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

    /** Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
      *
      * This feature - the ability to assign multiple rows a single `Id` computed over those all
      * of those rows - is why ''this'' method is the abstract primitive (and not [[append]]).
      *
      * Appends to the backing store whether or not there is a duplicate (no checking).
      *
      * FIXME: not thread safe: put a `Ref` based queue in front
      */
    final protected def append(row: Row, rows: Row*): F[Id] =
      for {
        id <- F delay nextId(row, rows: _*)
        rs <- F delay (row +: rows).toList
        _  <- cacheFill(id, rs)
        _  <- (Stream evals (F delay { rs map (id -> _) }) through persist).compile.drain
      } yield id
  }
}

/** Placeholder.
  */
object Stores
