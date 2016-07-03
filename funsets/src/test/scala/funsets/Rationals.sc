class Rational(numer: Int, denom: Int) {
  def n = numer

  def d = denom

  override def toString = n + "/" + d;

  def neg = new Rational(-n, d)

  def recip = new Rational(d, n)

  def add(that: Rational) = new Rational(n * that.d + that.n * d, d * that.d)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) = new Rational(n * that.n, d * that.d)

  def div(that: Rational) = mul(that.recip)
}

val x = new Rational(1, 2);
val y = new Rational(2, 3);
val z = new Rational(3, 2);
println(x.add(y));
println(x.sub(y));
println(x.sub(y).neg);
println(x.mul(y));
println(x.div(y));
println(y.div(x))




