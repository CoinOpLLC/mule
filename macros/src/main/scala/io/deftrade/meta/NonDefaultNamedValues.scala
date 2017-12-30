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

// no package chaining: does not depend on core, which implements io.deftrade api
// DO NOT REMOVE THIS COMMENT

package io.deftrade.meta

import scala.language.experimental.macros

abstract class NonDefaultNamedValues(val start: String, val sep: String, val end: String) {

  // val start = "("
  // val sep   = ", "
  // val end   = ")"

  /**
    * Use to override toString method for case classes to print field names as well as field values,
    * for only those field values which are not equal to the default value (if any) specified in the
    * apply() method of the companion.
    *
    * e.g.
    * {{{
    *   case class Foo(i: Int, s: String = "bar") {
    *     override def toString = NonDefaultNamedValues.nonDefaultNamedValues
    *   }
    * }}}
    * will generate this:
    * {{{
    *     override def toString = {
    *       val b = List.newBuilder[String]
    *       b += "i=" + this.i
    *       if (this.s != Foo.apply$default$2) b += "s=" + this.s
    *       b.result mkString (this.productPrefix + "[", ";", "]")
    *     }
    * }}}
    *
    * Implementation:
    *   - identify the companion object for the case class.
    *   - For each parameter of the apply method of the companion,
    *       - identify the field with the same `name`.
    *       - quote an expression which will produce a `"$``=$value"` string
    *       - If there is no default value,
    *           - return that expression
    *       - else
    *           - return an expression which conditions the production of the string on the value differening from the default.
    */
  def impl2(c: reflect.macros.blackbox.Context): c.Expr[String] = {

    import c.universe._

    val companionSymbol   = c.internal.enclosingOwner.owner.companion
    val companionTermName = companionSymbol.name.toTermName
    val companionType     = companionSymbol.typeSignature
    val applySymbol       = companionType.decl(TermName("apply")).asMethod

    val nvTrees =
      for {
        params         <- applySymbol.paramLists take 1
        (param, index) <- params.zipWithIndex
      } yield {
        val fieldNameString    = param.name.decodedName.toString
        val fieldTermName      = param.asTerm.name.toTermName
        val nvAppendTree       = q"""b += $fieldNameString + "=" + this.$fieldTermName"""
        val defaultValTermName = TermName(s"apply$$default$$${index + 1}")
        companionType member defaultValTermName match {
          case NoSymbol =>
            nvAppendTree
          case _ =>
            q"if (this.$fieldTermName != $companionTermName.$defaultValTermName) $nvAppendTree"
        }
      }

    c.Expr[String] {
      q"""
      val b = List.newBuilder[String]
      ..$nvTrees
      b.result mkString (this.productPrefix + $start, $sep, $end)
      """
    }
  }
}

/**
  * Default behavior is to match formatting style of case classes.
  */
object NonDefaultNamedValues extends NonDefaultNamedValues("(", ",", ")") {

  def nonDefaultNamedValues: String = macro impl2
}

object FintechNDNVs extends NonDefaultNamedValues("[", "; ", "]") {
  def muhNDNVs: String = macro impl2
}
