package net.fyrie
package ratio

import math.{ScalaNumber, ScalaNumericConversions}

object Ratio {

  def apply(in: String): Ratio[BigInt] = BigRatio(in)

  def apply(n: BigInt): Ratio[BigInt] = BigRatio(n)

  def apply(n: BigInt, d: BigInt): Ratio[BigInt] = BigRatio(n, d)

  def unapply(in: Any): Option[(BigInt,BigInt)] = BigRatio.unapply(in)

  def zero = BigRatio.zero

  def one = BigRatio.one

}

sealed abstract class Ratio[A: Integral] (val n: A, val d: A) extends ScalaNumber with ScalaNumericConversions with Ordered[Ratio[A]] {
  protected def ops: Integral[A] = implicitly

  def *(that: Ratio[A]): Ratio[A]
  def /(that: Ratio[A]): Ratio[A]
  def +(that: Ratio[A]): Ratio[A]
  def -(that: Ratio[A]): Ratio[A]

  def *[T <% A](that: T): Ratio[A]
  def /[T <% A](that: T): Ratio[A]
  def +[T <% A](that: T): Ratio[A]
  def -[T <% A](that: T): Ratio[A]

  def compare(that: Ratio[A]): Int = ops.compare((ops.times(n, that.d)), (ops.times(that.n, d)))

  override def toString = if (ops.gt(d, ops.one)) (n.toString + " / " + d.toString) else (n.toString)

  //override def hashCode: Int = 37 * (37 + (n % BigInt(Int.MaxValue)).toInt) + (d % BigInt(Int.MaxValue)).toInt

  override def equals(in: Any): Boolean = in match {
    case x: Ratio[_] => n == x.n && d == x.d
    case x: Int => n == x && d == 1
    case x: Long => n == x && d == 1
    case x: BigInt => n == x && d == 1
    case _ => false
  }

  def underlying(): AnyRef = (n,d)

  def isWhole = false
}

object BigRatio {
  def apply(in: String): Ratio[BigInt] =
    in.split("/").toList match {
      case n :: d :: Nil => apply(BigInt(n),BigInt(d))
      case n :: Nil => apply(BigInt(n))
      case _ => apply(0)
    }

  def apply(n: BigInt): Ratio[BigInt] = new BigRatio(n, 1)

  def apply(n: BigInt, d: BigInt): Ratio[BigInt] = d.signum match {
      case -1 => apply(-n, -d)
      case 0 => throw new IllegalArgumentException("Zero denominator")
      case 1 if (n == 1 || d == 1) => new BigRatio(n, d)
      case _ => 
        val g = n.gcd(d)
        new BigRatio(n / g, d / g)
  }

  def unapply(in: Any): Option[(BigInt,BigInt)] = in match {
    case BigRatio(n, d) => Some((n, d))
    case _ => None
  }

  val zero = BigRatio(0,1)

  val one = BigRatio(1,1)
}

final class BigRatio private (n: BigInt, d: BigInt) extends Ratio[BigInt](n, d) {
  def *(that: Ratio[BigInt]): Ratio[BigInt] = BigRatio(n * that.n, d * that.d)
  def /(that: Ratio[BigInt]): Ratio[BigInt] = BigRatio(n * that.d, d * that.n)
  def +(that: Ratio[BigInt]): Ratio[BigInt] = BigRatio((n * that.d) + (that.n * d), d * that.d)
  def -(that: Ratio[BigInt]): Ratio[BigInt] = BigRatio((n * that.d) - (that.n * d), d * that.d)

  def *[T <% BigInt](that: T): Ratio[BigInt] = this * BigRatio(that)
  def /[T <% BigInt](that: T): Ratio[BigInt] = this / BigRatio(that)
  def +[T <% BigInt](that: T): Ratio[BigInt] = this + BigRatio(that)
  def -[T <% BigInt](that: T): Ratio[BigInt] = this - BigRatio(that)

  override def hashCode: Int = 37 * (37 + (n % BigInt(Int.MaxValue)).toInt) + (d % BigInt(Int.MaxValue)).toInt

  def intValue = (n / d).toInt

  def longValue = (n / d).toLong

  def floatValue = (BigDecimal(n) / BigDecimal(d)).toFloat

  def doubleValue = (BigDecimal(n) / BigDecimal(d)).toDouble

}
