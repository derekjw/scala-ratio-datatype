package net.fyrie
package ratio

object Ratio {

  def apply(in: String): Ratio =
    in.split("/").toList match {
      case n :: d :: Nil => apply(BigInt(n),BigInt(d))
      case n :: Nil => apply(BigInt(n))
      case _ => apply(0)
    }

  def apply(n: BigInt): Ratio = apply(n, 1)

  def apply(n: BigInt, d: BigInt): Ratio = {
    
    def gcd(x: BigInt, y: BigInt): BigInt = if (x == 0) y else gcd(y % x, x)

    val m = if (d < 0) (-1) else 1
    val g: BigInt = if (n == 1 || d == 1) (1) else (gcd(n.abs, d.abs))

    if (g == 0) (new Ratio(0,0)) else (new Ratio(n * m / g, d * m / g))
  }

  def unapply(in: Any): Option[(BigInt,BigInt)] = in match {
    case Ratio(n, d) => Some((n, d))
    case _ => None
  }

}

class Ratio(val n: BigInt, val d: BigInt) extends Ordered[Ratio] {

/*  def *[T: Integral](that: T): Ratio = this * Ratio(that)
  def /[T: Integral](that: T): Ratio = this / Ratio(that)
  def +[T: Integral](that: T): Ratio = this + Ratio(that)
  def -[T: Integral](that: T): Ratio = this - Ratio(that)*/

  def *(that: Ratio): Ratio = Ratio(n * that.n, d * that.d)
  def /(that: Ratio): Ratio = Ratio(n * that.d, d * that.n)
  def +(that: Ratio): Ratio = Ratio((n * that.d) + (that.n * d), d * that.d)
  def -(that: Ratio): Ratio = Ratio((n * that.d) - (that.n * d), d * that.d)

  def compare(that: Ratio): Int = (n * that.d).compare(that.n * d)

  override def toString = if (d > 1) (n.toString + " / " + d.toString) else (n.toString)

  override def hashCode: Int = 37 * (37 * 17 * (n % BigInt(Int.MaxValue)).toInt) * (d % BigInt(Int.MaxValue)).toInt

  override def equals(in: Any): Boolean = in match {
    case Ratio(a,b) if n == a && d == b => true
    case _ => false
  }

}
