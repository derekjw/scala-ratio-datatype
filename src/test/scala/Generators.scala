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

  def genLongRatioPair: Gen[(Long, Long)] = for {
    n <- arbitrary[Long] suchThat (_ > Long.MinValue)
    d <- arbitrary[Long] suchThat (_ > Long.MinValue)
  } yield (n,d)

  def genTwoEqualLongRatioPairs: Gen[(Long, Long, Long, Long)] = for {
    n <- Gen.choose(-1000000,1000000)
    d <- Gen.choose(-1000000,1000000)
    m <- Gen.choose(1,10000000)
  } yield (n,d,n*m,d*m)

  def genLongRatio: Gen[Ratio] = for {
    (n, d) <- genLongRatioPair
  } yield Ratio(n,d)

  def genBigRatioPair: Gen[(BigInt, BigInt)] = for {
    n <- arbitrary[Long]
    d <- arbitrary[Long]
  } yield (n,d)

  def genTwoEqualBigRatioPairs: Gen[(BigInt, BigInt, BigInt, BigInt)] = for {
    n <- Gen.choose(-1000000000000L,100000000000L)
    d <- Gen.choose(-1000000000000L,100000000000L)
    m <- Gen.choose(1,10000000000L)
  } yield (n,d,n*m,d*m)

  def genBigRatio: Gen[Ratio] = for {
    (n, d) <- genBigRatioPair
  } yield Ratio(n,d)

  def genRatio: Gen[Ratio] = genIntRatio | genLongRatio | genBigRatio

  implicit def arbRatio: Arbitrary[Ratio] =
    Arbitrary { genRatio }

}
