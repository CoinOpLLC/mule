// package io.deftrade
// package test
//
// import org.scalatest._, prop._
//
// ??? // FIXME way of out date evidently
// class ExampleFlatSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
//
//   "A Stack" should "pop values in last-in-first-out order" in {
//     var stack = List.empty[Int]
//     stack = 2 :: 1 :: stack
//     stack.head should be(2)
//     stack = stack.tail
//     stack.head should be(1)
//   }
//
//   it should "throw NoSuchElementException if an empty stack is popped" in {
//     val emptyStack = List.empty[Int]
//     a[NoSuchElementException] should be thrownBy {
//       emptyStack.head
//     }
//   }
//
//   "The integers" should "generally behave as advertized" in {
//     forAll { n: Int =>
//       whenever(n > 1) { n / 2 should be > 0 }
//     }
//   }
// }
//
// class ExamplePropSpec extends PropSpec with ScalaCheckDrivenPropertyChecks {
// // with TableDrivenPropertyChecks {
//   property("unit is as unit does") {
//     forAll { ewie: Unit =>
//       assert(ewie === (()))
//     }
//   }
//   import org.scalacheck.Gen._
//
//   property("even ints are even") {
//     forAll { i: Int =>
//       whenever((i & 1) === 0) {
//         assert(i % 2 === 0)
//       }
//     }
//   }
//
//   property("doubles gonna double") {
//     forAll { xs: List[Double] =>
//       whenever(xs.nonEmpty) {
//         import scala.language.postfixOps
//         assert(math.sqrt(xs map (x => x * x) sum) >= 0)
//       }
//     }
//   }
// }
