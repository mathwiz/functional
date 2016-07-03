val x = new Rational(1, 3);
val y = new Rational(5, 7);
val z = new Rational(3, 2);
x.add(y)
x.sub(y)
x.sub(y).neg
x.mul(y)
x.div(y)
y.div(x)
x.sub(y).sub(z)
y.add(y)
x.less(y)
x.max(y)
y.max(x)
x.mul(new Rational(3))

//new Rational(1,0)
//
class Rational(numer: Int, denom: Int) {
  require(denom != 0, "denominator must not be zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(numer, denom)

  def this(numer: Int) = this(numer, 1)

  val n = numer / g

  val d = denom / g

  override def toString = if (d == 1) "" + n else n + "/" + d;

  def max(that: Rational) = if (less(that)) that else this

  def less(that: Rational) = n * that.d < that.n * d

  def equals(that: Rational) = n * that.d == that.n * d

  def greater(that: Rational) = !less(that) && !equals(that)

  def neg = new Rational(-n, d)

  def recip = new Rational(d, n)

  def add(that: Rational) = new Rational(n * that.d + that.n * d, d * that.d)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) = new Rational(n * that.n, d * that.d)

  def div(that: Rational) = mul(that.recip)
}




