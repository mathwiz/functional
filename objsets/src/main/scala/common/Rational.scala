package common

/**
  * Created by Yohan on 7/10/2016.
  */
class Rational(n: Int, d: Int) {
  require(d != 0, "denominator must not be zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(n, d)

  def this(numer: Int) = this(numer, 1)

  val numer = n / g

  val denom = d / g

  override def toString =
    if (numer < 0 && denom > 0) "-" + numer.unary_- + "/" + denom
    else if (numer > 0 && denom < 0) "-" + numer + "/" + denom.unary_-
    else if (denom == 1) "" + numer
    else "" + numer + "/" + denom

  def max(that: Rational) = if (<(that)) that else this

  def <(that: Rational) = numer * that.denom < that.numer * denom

  def ==(that: Rational) = numer * that.denom == that.numer * denom

  def >(that: Rational) = ! <(that) && ! ==(that)

  def unary_- = new Rational(-numer, denom)

  def recip = new Rational(denom, numer)

  def +(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def -(that: Rational) = this.+(that.unary_-)

  def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  def /(that: Rational) = this.*(that.recip)
}