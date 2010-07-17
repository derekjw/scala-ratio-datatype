package net.fyrie

import ratio._

import org.scalacheck._

import Gen._
import Arbitrary.arbitrary

import collection.IndexedSeq

object Primes {
  
  def ints(n: Int): Stream[Int] = Stream.cons(n, ints(n +1))
  
  def primes(nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head,
                primes ((nums tail) filter (x => nums.head != 0)))

  def apply(count: Int) =
    IndexedSeq[Int]() ++ primes(ints(2)).take(count)
}

object Generators {
  val primes = Primes(1000)
  val smallPrimes = primes.take(20)

  def genSmallPrime: Gen[Int] = Gen(params => Some(smallPrimes(params.rng.nextInt(smallPrimes.size))))
  def genPrime: Gen[Int] = Gen(params => Some(primes(params.rng.nextInt(primes.size))))

  def genFactors(min: Int, max: Int): Gen[Seq[Int]] = for {
    size <- Gen.choose(min, max)
    result <- Gen.listOfN(size, Gen.frequency((1,genPrime),(9,genSmallPrime)))
  } yield result

  def genBigRatio(factors: Int): Gen[Ratio[BigInt]] = genBigRatio(1,factors)

  def genBigRatio(minFactors: Int, maxFactors: Int): Gen[Ratio[BigInt]] = for {
    n <- genFactors(minFactors, maxFactors)
    d <- genFactors(minFactors, maxFactors)
  } yield {
    n.zip(d).foldLeft(BigRatio.one){
      case (r,(n,d)) => r * BigRatio(n, d)
    }
  }
}
