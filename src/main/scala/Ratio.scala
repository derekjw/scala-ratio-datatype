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

  def apply(in: String): IntRatio =
    in.split("/").toList match {
      case n :: d :: Nil => apply(n.toInt,d.toInt)
      case n :: Nil => apply(n.toInt)
      case _ => apply(0)
    }

  def unapply[T](in: Ratio[T]): Option[(T,T)] = Some((in.n, in.d))

}

sealed abstract class Ratio[T] (val n: T, val d: T) {

  protected val ops: Integral[T]

  override def toString = if (ops.gt(d, ops.one)) (n.toString + " / " + d.toString) else (n.toString)

  override def hashCode: Int = 37 * (37 * 17 * ops.toInt(ops.rem(n, ops.fromInt(Int.MaxValue)))) * ops.toInt(ops.rem(d, ops.fromInt(Int.MaxValue)))

  override def equals(in: Any): Boolean = in match {
    case Ratio(a,b) if n == a && d == b => true
    case _ => false
  }

  def toIntRatio = new IntRatio(n, d)

  def toLongRatio = new LongRatio(n, d)

  implicit def type2Int(in: T): Int = ops.toInt(in)

  implicit def type2Long(in: T): Long = ops.toLong(in)
}

class IntRatio(n: Int, d: Int) extends Ratio[Int](n, d) {
  val ops = IntIsIntegral

  override def toIntRatio = this
}

class LongRatio(n: Long, d: Long) extends Ratio[Long](n, d) {
  val ops = LongIsIntegral

  override def toLongRatio = this
}

