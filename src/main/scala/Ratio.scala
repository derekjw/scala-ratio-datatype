package net.fyrie
package ratio

import org.apache.commons.math.util.MathUtils

import math.Numeric._

object Ratio {

  def apply(n: Int, d: Int): IntRatio = {
    val m = if (d < 0) (-1) else 1
    val g = if (n == 1 || d == 1) (1) else (MathUtils.gcd(n, d))
      if (g == 0) (new IntRatio(0,0)) else (new IntRatio(m * n / g, m * d / g))
  }

  def apply(n: Int): IntRatio = apply(n, 1)

  def apply(n: Long, d: Long): LongRatio = {
    val m = if (d < 0) (-1) else 1
    val g = if (n == 1 || d == 1) (1) else (MathUtils.gcd(n, d))
      if (g == 0) (new LongRatio(0,0)) else (new LongRatio(m * n / g, m * d / g))
  }

  def apply(n: Long): LongRatio = apply(n, 1)

  def apply(n: BigInt, d: BigInt): BigRatio = {
    val m: BigInt = if (d < 0) (-1) else 1
    val g: BigInt = if (n == 1 || d == 1) (1) else (n.gcd(d))
      if (g == 0) (new BigRatio(0,0)) else (new BigRatio(m * n / g, m * d / g))
  }

  def apply(n: BigInt): BigRatio = apply(n, 1)

  def apply(in: String): Ratio =
    in.split("/").toList match {
      case n :: d :: Nil => optimize(apply(BigInt(n),BigInt(d)))
      case n :: Nil => optimize(apply(BigInt(n)))
      case _ => apply(0)
    }

  def unapply(in: Any): Option[(Any,Any)] = in match {
    case IntRatio(n, d) => Some((n, d))
    case LongRatio(n, d) => Some((n, d))
    case BigRatio(n, d) => Some((n, d))
    case _ => None
  }

  def optimize(in: Ratio): Ratio = in match {
    case BigRatio(n,d) if List(n,d).forall(x => (BigInt(Long.MaxValue) >= x) && (BigInt(Long.MinValue) <= x)) => optimize(in.toLongRatio)
    case LongRatio(n,d) if List(n,d).forall(x => (Int.MaxValue.toLong >= x) && (Int.MinValue.toLong <= x)) => in.toIntRatio
    case _ => in
  }
}

sealed abstract class Ratio extends Ordered[Ratio] {
  type T

  val n: T

  val d: T

  protected val ops: Integral[T]

  def compare(that: Ratio): Int = toBigRatio.compare(that.toBigRatio)

  def *(that: Ratio): Ratio = Ratio.optimize(toBigRatio * that.toBigRatio)
  def /(that: Ratio): Ratio = Ratio.optimize(toBigRatio / that.toBigRatio)
  def +(that: Ratio): Ratio = Ratio.optimize(toBigRatio + that.toBigRatio)
  def -(that: Ratio): Ratio = Ratio.optimize(toBigRatio - that.toBigRatio)

  def *(that: Int): Ratio = this * Ratio(that)
  def /(that: Int): Ratio = this / Ratio(that)
  def +(that: Int): Ratio = this + Ratio(that)
  def -(that: Int): Ratio = this - Ratio(that)

  def *(that: Long): Ratio = this * Ratio(that)
  def /(that: Long): Ratio = this / Ratio(that)
  def +(that: Long): Ratio = this + Ratio(that)
  def -(that: Long): Ratio = this - Ratio(that)

  def *(that: BigInt): Ratio = this * Ratio(that)
  def /(that: BigInt): Ratio = this / Ratio(that)
  def +(that: BigInt): Ratio = this + Ratio(that)
  def -(that: BigInt): Ratio = this - Ratio(that)

  override def toString = if (ops.gt(d, ops.one)) (n.toString + " / " + d.toString) else (n.toString)

  override def hashCode: Int = 37 * (37 * 17 * ops.toInt(ops.rem(n, ops.fromInt(Int.MaxValue)))) * ops.toInt(ops.rem(d, ops.fromInt(Int.MaxValue)))

  override def equals(in: Any): Boolean = in match {
    case Ratio(a,b) if n == a && d == b => true
    case _ => false
  }

  def isInt: Boolean = false

  def isLong: Boolean = false

  def isBig: Boolean = false

  def toIntRatio = new IntRatio(ops.toInt(n), ops.toInt(d))

  def toLongRatio = new LongRatio(ops.toLong(n), ops.toLong(d))

  def toBigRatio: BigRatio

}

object IntRatio {
  def unapply(in: Any): Option[(Int,Int)] = in match {
    case r: IntRatio => Some((r.n, r.d))
    case _ => None
  }
}

class IntRatio(numer: Int, denom: Int) extends Ratio {
  type T = Int

  val ops = IntIsIntegral

  val n = numer

  val d = denom

  override def isInt: Boolean = true

  override def toIntRatio = this

  def toBigRatio = new BigRatio(n,d)
}

object LongRatio {
  def unapply(in: Any): Option[(Long,Long)] = in match {
    case r: LongRatio => Some((r.n, r.d))
    case _ => None
  }
}

class LongRatio(numer: Long, denom: Long) extends Ratio {
  type T = Long

  val ops = LongIsIntegral

  val n = numer

  val d = denom

  override def isLong: Boolean = true

  override def toLongRatio = this

  def toBigRatio = new BigRatio(n,d)
}

object BigRatio {
  def unapply(in: Any): Option[(BigInt,BigInt)] = in match {
    case r: BigRatio => Some((r.n, r.d))
    case _ => None
  }
}

class BigRatio(numer: BigInt, denom: BigInt) extends Ratio {
  type T = BigInt

  val ops = BigIntIsIntegral

  val n = numer

  val d = denom

  def *(that: BigRatio): BigRatio = Ratio(n * that.n, d * that.d)
  def /(that: BigRatio): BigRatio = Ratio(n * that.d, d * that.n)
  def +(that: BigRatio): BigRatio = Ratio((n * that.d) + (that.n * d), d * that.d)
  def -(that: BigRatio): BigRatio = Ratio((n * that.d) - (that.n * d), d * that.d)

  def compare(that: BigRatio): Int = (n * that.d).compare(that.n * d)

  override def isBig: Boolean = true

  override def toBigRatio = this
}

