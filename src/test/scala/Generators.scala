package net.fyrie

import ratio._

import org.scalacheck._

import Gen._
import Arbitrary.arbitrary

object Generators {

  def genIntRatioPair: Gen[(Int, Int)] = for {
    n <- arbitrary[Int] suchThat (_ > java.lang.Integer.MIN_VALUE)
    d <- arbitrary[Int] suchThat (_ > java.lang.Integer.MIN_VALUE)
  } yield (n,d)

  def genTwoEqualIntRatioPairs: Gen[(Int, Int, Int, Int)] = for {
    n <- Gen.choose(-1000,1000)
    d <- Gen.choose(-1000,1000)
    m <- Gen.choose(1,10000)
  } yield (n,d,n*m,d*m)

  def genIntRatio: Gen[IntRatio] = for {
    (n, d) <- genIntRatioPair
  } yield Ratio(n,d)

  implicit def arbIntRatio: Arbitrary[IntRatio] =
    Arbitrary { genIntRatio }

}
