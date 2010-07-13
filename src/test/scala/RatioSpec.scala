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
      genRatio(100) must pass{ r: Ratio => { true } }(display(minTestsOk -> 1000, maxDiscarded -> 1, workers -> 4))
    }
    "Perform operations with Ints" in {
      ((Ratio(2,3) * 3) == 2) must beTrue
      ((Ratio(2,3) * 4) == 2) must beFalse
    }
  }
}
