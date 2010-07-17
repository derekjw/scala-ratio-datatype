package net.fyrie
package ratio
package specs

import Generators._

import org.specs._
import specification.Context

import org.scalacheck._
import org.scalacheck.Prop._

class RatioSpec extends Specification with ScalaCheck {
  "Ratios" should {
    "Allow any input" in {
      genBigRatio(100) must pass{ r: Ratio[BigInt] => { true } }(display(minTestsOk -> 1000, maxDiscarded -> 1, workers -> 4))
    }
    "Perform operations with Ints" in {
      ((BigRatio(2,3) * 3) == 2) must beTrue
      ((BigRatio(2,3) * 4) == 2) must beFalse
    }
  }
  "Equality" should {
      "Ints" in {
        (BigRatio(4,2) == 2) must beTrue
        (2 == BigRatio(4,2)) must beTrue
        (BigRatio(4,3) == 2) must beFalse
        (2 == BigRatio(4,3)) must beFalse
      }
  }
}
