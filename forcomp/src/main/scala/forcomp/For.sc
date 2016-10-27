primePairs(7)
val a = Vector(1.2, 1.3, 1.4)
val b = Vector(.4, .3, .2)
scalarProduct(a, b)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum

def primePairs(n: Int) =
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)


def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)
