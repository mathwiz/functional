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
new Rational(1, 2).numer
new Rational(1, 2).less(new Rational(2, 3))

//new Rational(1,0)
//
class Rational(n: Int, d: Int) {
  require(d != 0, "denominator must not be zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(n, d)

  def this(numer: Int) = this(numer, 1)

  val numer = n / g

  val denom = d / g

  override def toString = if (denom == 1) "" + numer else numer + "/" + denom;

  def max(that: Rational) = if (less(that)) that else this

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def equals(that: Rational) = numer * that.denom == that.numer * denom

  def greater(that: Rational) = !less(that) && !equals(that)

  def neg = new Rational(-numer, denom)

  def recip = new Rational(denom, numer)

  def add(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  def div(that: Rational) = mul(that.recip)
}




