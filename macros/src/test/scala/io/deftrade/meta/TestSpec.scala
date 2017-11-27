package io.deftrade
package meta

import org.scalatest.{ FlatSpec, Matchers }

case class Foo(i: Int = 0, s: String = "", d: Double = math.E) {
  // import NonDefaultNamedValues.{ nonDefaultNamedValues => ndnvs }
  import FintechNDNVs.{ muhNDNVs => ndnvs }
  override def toString = ndnvs
}

class NdnvSpec extends FlatSpec with Matchers {
  "NDNVs" should "work again" in {
    val y7 = Foo(i = 6, s = "wtf")
    y7.toString should ===("Foo[i=6; s=wtf]")
  }
}
