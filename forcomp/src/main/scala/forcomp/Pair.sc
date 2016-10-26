val a = List(1, 31, 41, 42, 43, 61)
val b = List(10, 20, 30, 40, 50, 60)
merge(a, b)
val c = a ++ b
mergesort(c)

def mergesort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n==0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(mergesort(fst), mergesort(snd))
  }
}

def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Nil, yh::yt) => ys
    case (xh::xt, Nil) => xs
    case (xh::xt, yh::yt) =>
      if (xh < yh) xh :: merge(xt, ys) else yh :: merge(xs, yt)
  }