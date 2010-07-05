package net.fyrie
package ratio

object Ratio {

  def apply(in: String): Ratio =
    in.split("/").toList match {
      case n :: d :: Nil => optimize(apply(BigInt(n),BigInt(d)))
      case n :: Nil => optimize(apply(BigInt(n)))
      case _ => apply(0)
    }

  def apply[T: Integral](n: T): Ratio = apply(n, implicitly[Integral[T]].one)

  def apply[T: Integral](n: T, d: T): Ratio = {
    val integral: Integral[T] = implicitly
    import integral._

    implicit def int2T(in: Int): T = fromInt(in)
    
    def gcd(x: T, y: T): T = if (x == 0) y else gcd(y % x, x)

    val m: T = if (d < 0) (-1) else 1
    val g: T = if (n == 1 || d == 1) (1) else (gcd(abs(n), abs(d)))

    optimize((if (g == 0) ((0,0)) else ((m * n / g, m * d / g))) match {
      case (x: Int, y: Int) => new IntRatio(x, y)
      case (x: Long, y: Long) => new LongRatio(x, y)
      case (x: BigInt, y: BigInt) => new BigRatio(x, y)
    })
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
  type NumberType

  val n: NumberType

  val d: NumberType

  val integral: Integral[NumberType]
  import integral._

  def compare(that: Ratio): Int = toBigRatio.compare(that.toBigRatio)

  def *(that: Ratio): Ratio = Ratio.optimize(toBigRatio * that.toBigRatio)
  def /(that: Ratio): Ratio = Ratio.optimize(toBigRatio / that.toBigRatio)
  def +(that: Ratio): Ratio = Ratio.optimize(toBigRatio + that.toBigRatio)
  def -(that: Ratio): Ratio = Ratio.optimize(toBigRatio - that.toBigRatio)

  def *[T: Integral](that: T): Ratio = this * Ratio(that)
  def /[T: Integral](that: T): Ratio = this / Ratio(that)
  def +[T: Integral](that: T): Ratio = this + Ratio(that)
  def -[T: Integral](that: T): Ratio = this - Ratio(that)

  override def toString = if (gt(d, one)) (n.toString + " / " + d.toString) else (n.toString)

  override def hashCode: Int = 37 * (37 * 17 * toInt(rem(n, fromInt(Int.MaxValue)))) * toInt(rem(d, fromInt(Int.MaxValue)))

  override def equals(in: Any): Boolean = in match {
    case Ratio(a,b) if n == a && d == b => true
    case _ => false
  }

  def isInt: Boolean = false

  def isLong: Boolean = false

  def isBig: Boolean = false

  def toIntRatio = new IntRatio(toInt(n), toInt(d))

  def toLongRatio = new LongRatio(toLong(n), toLong(d))

  def toBigRatio: BigRatio

}

object IntRatio {
  def unapply(in: Any): Option[(Int,Int)] = in match {
    case r: IntRatio => Some((r.n, r.d))
    case _ => None
  }
}

class IntRatio(numer: Int, denom: Int) extends Ratio {
  type NumberType = Int

  val integral = implicitly[Integral[NumberType]]

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
  type NumberType = Long

  val integral = implicitly[Integral[NumberType]]

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
  type NumberType = BigInt

  val integral = implicitly[Integral[NumberType]]

  val n = numer

  val d = denom

  def *(that: BigRatio): Ratio = Ratio(n * that.n, d * that.d)
  def /(that: BigRatio): Ratio = Ratio(n * that.d, d * that.n)
  def +(that: BigRatio): Ratio = Ratio((n * that.d) + (that.n * d), d * that.d)
  def -(that: BigRatio): Ratio = Ratio((n * that.d) - (that.n * d), d * that.d)

  def compare(that: BigRatio): Int = (n * that.d).compare(that.n * d)

  override def isBig: Boolean = true

  override def toBigRatio = this
}

