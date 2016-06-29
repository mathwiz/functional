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

def product2(a: Int, b: Int) = general(x => 1)((x, y) => x * y)(x => x)(a, b);

def sum2(a: Int, b: Int) = general(x => 0)((x, y) => x + y)(x => x)(a, b);

sum2(1, 4);
product2(1, 4);

