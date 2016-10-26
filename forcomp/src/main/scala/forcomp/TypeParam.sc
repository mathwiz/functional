val a = List(1, 31, 41, 42, 43, 61)
val b = List(10, 20, 30, 40, 50, 60)
val intComp = (x: Int, y: Int) => x < y
merge(a, b)(intComp)
val c = a ++ b ::: List(-1, -2, -3)
mergesort(c)(intComp)
mergesort(List("apple", "pineapple", "banana", "orange", "kiwi"))((x, y) => x.compareTo(y) < 0)

def mergesort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(mergesort(fst)(lt), mergesort(snd)(lt))(lt)
  }
}

def merge[T](xs: List[T], ys: List[T])(lt: (T, T) => Boolean): List[T] =
  (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (xh :: xt, yh :: yt) =>
      if (lt(xh, yh)) xh :: merge(xt, ys)(lt) else yh :: merge(xs, yt)(lt)
  }