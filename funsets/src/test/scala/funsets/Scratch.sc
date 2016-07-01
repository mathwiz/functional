def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

val x = product(x => x)(1, 4);

def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)

val y = sum(x => x)(1, 4);

def general(i: Int => Int)(g: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) i(0)
  else g(f(a), general(i)(g)(f)(a + 1, b))
}

general(x => 0)((x, y) => x + y)(x => x)(1, 4);

general(x => 1)((x, y) => x * y)(x => x)(1, 4);

def product2(f: Int => Int)(a: Int, b: Int) = general(x => 1)((x, y) => x * y)(f)(a, b);

def sum2(f: Int => Int)(a: Int, b: Int) = general(x => 0)((x, y) => x + y)(f)(a, b);

sum2(x => x)(1, 4);
product2(x => x)(1, 4);

def sumOfSquares(a: Int, b: Int): Int =
  sum2(x => x * x)(a, b)

sumOfSquares(1, 3);

def averageDamp(f: Double => Double)(x: Double): Double =
  (x + f(x)) / 2.0

import math.abs;

def fixedPoint(f: Double => Double)(x: Double): Double = {
  val tolerance = 0.0001;
  def iterate(y: Double): Double = {
    val next = averageDamp(f)(y);
    if (abs(y - next) / y < tolerance) next
    else iterate(next)
  }
  iterate(x);
}

def sqrt(x: Double) =
  fixedPoint(y => x / y)(x)

sqrt(4);
sqrt(2);





