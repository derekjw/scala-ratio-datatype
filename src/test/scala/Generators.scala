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

  def genIntRatio: Gen[IntRatio] = for {
    (n, d) <- genIntRatioPair
  } yield Ratio(n,d)

  implicit def arbIntRatio: Arbitrary[IntRatio] =
    Arbitrary { genIntRatio }

  def genLongRatioPair: Gen[(Long, Long)] = for {
    n <- arbitrary[Long] suchThat (_ > Long.MinValue)
    d <- arbitrary[Long] suchThat (_ > Long.MinValue)
  } yield (n,d)

  def genTwoEqualLongRatioPairs: Gen[(Long, Long, Long, Long)] = for {
    n <- Gen.choose(-1000000,1000000)
    d <- Gen.choose(-1000000,1000000)
    m <- Gen.choose(1,10000000)
  } yield (n,d,n*m,d*m)

  def genLongRatio: Gen[LongRatio] = for {
    (n, d) <- genLongRatioPair
  } yield Ratio(n,d)

  implicit def arbLongRatio: Arbitrary[LongRatio] =
    Arbitrary { genLongRatio }

  def genRatio: Gen[Ratio] = genIntRatio | genLongRatio

  implicit def arbRatio: Arbitrary[Ratio] =
    Arbitrary { genRatio }

}
