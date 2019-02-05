package io.deftade
package model

import eu.timepit.refined
import io.circe.Json

import refined.W
import refined.api.Refined
import refined.boolean.{ And, Or }
import refined.collection.{ MaxSize, NonEmpty }
import refined.string.{ MatchesRegex, Trimmed }
import refined.auto._
