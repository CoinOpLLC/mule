package io.deftrade
package model

import keyval._, refinements._

// import cats._
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.collection.NonEmpty

/** */
object reference {

  /** */
  sealed abstract case class CountryCode(
      alpha2: Alpha2,
      alpha3: Alpha3,
      countryCode: Num3,
      name: String Refined NonEmpty,
      regionCode: Option[Num3],
      subRegionCode: Option[Num3],
      intermediateRegionCode: Option[Num3],
      region: VarChar,
      subRegion: VarChar,
      intermediateRegion: VarChar,
      // iso_3166_2: NonEmptyVarChar,
  )

  /** */
  object CountryCode extends WithOpaqueKey[Int, CountryCode] {

    def regions: Map[Num3, VarChar]             = ???
    def subRegions: Map[Num3, VarChar]          = ???
    def intermediateRegions: Map[Num3, VarChar] = ???

    def apply(alpha2: Alpha2,
              alpha3: Alpha3,
              countryCode: Num3,
              name: String Refined NonEmpty,
              regionCode: Option[Num3],
              subRegionCode: Option[Num3],
              intermediateRegionCode: Option[Num3]): CountryCode =
      new CountryCode(
        alpha2,
        alpha3,
        countryCode,
        name,
        regionCode,
        subRegionCode,
        intermediateRegionCode,
        region = (regions get countryCode).fold(VarChar.empty)(identity),
        subRegion = (regions get countryCode).fold(VarChar.empty)(identity),
        intermediateRegion = (regions get countryCode).fold(VarChar.empty)(identity),
        // s"ISO 3166-2:$alpha2",
      ) {}

  }
}
