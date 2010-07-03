package net.fyrie
package ratio
package specs

import Generators._

import org.specs._
import specification.Context

import org.scalacheck._
import org.scalacheck.Prop._

class RatioSpec extends Specification with ScalaCheck {
  "IntRatios" should {
    "Be equal with the same values" in {
      genIntRatioPair must pass{ p: (Int, Int) =>
        Ratio(p._1, p._2) must_== Ratio(p._1, p._2)
      }
    }
    "Be equal after reducing" in {
      genTwoEqualIntRatioPairs must pass{ p: (Int, Int, Int, Int) =>
        Ratio(p._1, p._2) must_== Ratio(p._3, p._4)
      }
    }
  }
  "Ratios" should {
    "Allow any input" in {
      genRatio must pass{ r: Ratio => { true } }(set(minTestsOk -> 100000, maxDiscarded -> 5000, workers -> 4))
    }
  }
}