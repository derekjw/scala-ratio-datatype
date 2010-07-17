package net.fyrie
package ratio

import math.{ScalaNumber, ScalaNumericConversions}

object Ratio {

  def apply(in: String): Ratio =
    in.split("/").toList match {
      case n :: d :: Nil => apply(BigInt(n),BigInt(d))
      case n :: Nil => apply(BigInt(n))
      case _ => apply(0)
    }

  def apply(n: BigInt): Ratio = new Ratio(n, 1)

  def apply(n: BigInt, d: BigInt): Ratio = {
    val m = d.signum
    val g: BigInt = if (n == 1 || d == 1) 1 else n.gcd(d)

    if (g == 0) (new Ratio(0,0)) else (new Ratio(n * m / g, d * m / g))
  }

  def unapply(in: Any): Option[(BigInt,BigInt)] = in match {
    case Ratio(n, d) => Some((n, d))
    case _ => None
  }

}

class Ratio(val n: BigInt, val d: BigInt) extends ScalaNumber with ScalaNumericConversions with Ordered[Ratio] {

  def *[T <% BigInt](that: T): Ratio = this * Ratio(that)
  def /[T <% BigInt](that: T): Ratio = this / Ratio(that)
  def +[T <% BigInt](that: T): Ratio = this + Ratio(that)
  def -[T <% BigInt](that: T): Ratio = this - Ratio(that)

  def *(that: Ratio): Ratio = Ratio(n * that.n, d * that.d)
  def /(that: Ratio): Ratio = Ratio(n * that.d, d * that.n)
  def +(that: Ratio): Ratio = Ratio((n * that.d) + (that.n * d), d * that.d)
  def -(that: Ratio): Ratio = Ratio((n * that.d) - (that.n * d), d * that.d)

  def compare(that: Ratio): Int = (n * that.d).compare(that.n * d)

  override def toString = if (d > 1) (n.toString + " / " + d.toString) else (n.toString)

  override def hashCode: Int = 37 * (37 + (n % BigInt(Int.MaxValue)).toInt) + (d % BigInt(Int.MaxValue)).toInt

  override def equals(in: Any): Boolean = in match {
    case x: Ratio => (n == x.n && d == x.d) || (toDouble == x.toDouble)
    case x: Int => n == x && d == 1
    case x: Long => n == x && d == 1
    case x: BigInt => n == x && d == 1
    case _ => false
  }

  def underlying(): AnyRef = (n,d)

  def isWhole = false

  def intValue = (n / d).toInt

  def longValue = (n / d).toLong

  def floatValue = (BigDecimal(n) / BigDecimal(d)).toFloat

  def doubleValue = (BigDecimal(n) / BigDecimal(d)).toDouble

}
