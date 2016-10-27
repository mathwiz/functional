combinations(3, 2)
val a = Vector(1.2, 1.3, 1.4)
val b = Vector(.4, .3, .2)
scalarProduct(a, b)
isPrime(23)
isPrime(24)
primePairs(7)

def primePairs(n: Int) =
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys) map (xy => xy._1 * xy._2) sum

def combinations(M: Int, N: Int): Seq[(Int, Int)] =
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))

