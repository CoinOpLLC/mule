/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
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

package wut

import scala.language.experimental.macros

trait NonDefaultNamedValues {

  //  val start = '('
  //  val sep = ", "
  //  val end = ')'

  val start = '['
  val sep   = "; "
  val end   = ']'

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
    *     b += "i=" + this.i
    *     if (this.s != Foo.apply$default$2) b += "s=" + this.s
    *     b.result mkString (this.productPrefix + "[", ";", "]")
    *   }
    * }}}
    *
    * Implementation: identify the companion object for the case class.
    * For each parameter of the apply method of the companion,
    * identify the field with the same name.
    * If the field has a value which is not equal to the default value for the corresponding
    * parameter in the apply method, or there is no default value, emit a "name=value" string
    * for the toString method.
    */
  def impl2(c: reflect.macros.blackbox.Context): c.Expr[String] = {

    import c.universe._

    val companionSymbol   = c.internal.enclosingOwner.owner.companion
    val companionTermName = companionSymbol.name.toTermName
    val companionType     = companionSymbol.typeSignature
    val applySymbol       = companionType.decl(TermName("apply")).asMethod

    val builderTree = q"val b = List.newBuilder[String]"

    val nvTrees =
      for {
        params         <- applySymbol.paramLists
        (param, index) <- params.zipWithIndex
        defaultValTermName = TermName(s"apply$$default$$${index + 1}")
        fieldNameString    = param.name.decodedName.toString
        fieldTermName      = param.asTerm.name.toTermName
        nvAppendTree       = q"""b += $fieldNameString + "=" + this.$fieldTermName"""
      } yield
        companionType member defaultValTermName match {
          case NoSymbol =>
            nvAppendTree
          case _ =>
            q"if (this.$fieldTermName != $companionTermName.$defaultValTermName) $nvAppendTree"
        }

    val mkStringTree = q"""b.result mkString (this.productPrefix + "[", "; ", "]")"""

    c.Expr[String] {
      q"""
      $builderTree
      ..$nvTrees
      $mkStringTree
      """
    }
  }
}

object NonDefaultNamedValues extends NonDefaultNamedValues {

  def nonDefaultNamedValues: String = macro impl2
}
