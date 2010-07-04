package net.fyrie

import ratio._

import org.scalacheck._

import Gen._
import Arbitrary.arbitrary

object Generators {

  def genIntRatioPair: Gen[(Int, Int)] = for {
    n <- arbitrary[Int] suchThat (_ > Int.MinValue)
    d <- arbitrary[Int] suchThat (_ > Int.MinValue)
  } yield (n,d)

  def genTwoEqualIntRatioPairs: Gen[(Int, Int, Int, Int)] = for {
    n <- Gen.choose(-1000,1000)
    d <- Gen.choose(-1000,1000)
    m <- Gen.choose(1,10000)
  } yield (n,d,n*m,d*m)

  def genIntRatio: Gen[Ratio] = for {
    (n, d) <- genIntRatioPair
  } yield Ratio(n,d)

  implicit def arbIntRatio: Arbitrary[Ratio] =
    Arbitrary { genIntRatio }

  def genRatio: Gen[Ratio] = genIntRatio

  implicit def arbRatio: Arbitrary[Ratio] =
    Arbitrary { genRatio }

}
