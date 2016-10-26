val a = List(1, 2, 3, 4, 5, 6)
map(a)(x => 2 * x)
scaleList(map(a)(x => x * 1.0), Math.pow(.5, 3))
squareListPat(a)
squareListMap(a)
filter(a)(x => x % 2 == 0)
pack(List("a", "a", "a", "b", "c", "c", "a"))
encode(List("a", "a", "a", "b", "c", "c", "a"))
reduceLeft(a, (x: Int, y: Int) => x + y)
foldLeft(a, 0)((x, y) => x + y)
reduceRight(a, (x: Int, y: Int) => x + y)
foldRight(a, 0)((x, y) => x + y)
concat(a, List(8, 6, 4))
mapFun(a, (x: Int) => 5 * x)
lengthFun(a)

def lengthFun[T](xs: List[T]): Int =
  foldRight(xs, 0)((x, y) => 1 + y)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  foldRight(xs, List[U]())((x, y) => f(x) :: y)

def concat[T](xs: List[T], ys: List[T]): List[T] =
  foldRight(xs, ys)((x, y) => x :: y)

def reduceRight[T](xs: List[T], op: (T, T) => T): T = xs match {
  case Nil => throw new Error("Nil.reduceRight")
  case x :: Nil => x
  case h :: t => op(h, reduceRight(t, op))
}

def foldRight[T, U](xs: List[T], z: U)(op: (T, U) => U): U = xs match {
  case Nil => z
  case h :: t => op(h, foldRight(t, z)(op))
}

def reduceLeft[T](xs: List[T], op: (T, T) => T): T = xs match {
  case Nil => throw new Error("Nil.reduceLeft")
  case h :: t => foldLeft(t, h)(op)
}

def foldLeft[T, U](xs: List[T], z: U)(op: (U, T) => U): U = xs match {
  case Nil => z
  case h :: t => foldLeft(t, op(z, h))(op)
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (ps => (ps.head, ps.length))
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case h :: Nil => List(xs)
  case h :: t =>
    val (sames, rest) = t span (x => x == h)
    List(h) ++ sames :: pack(rest)
}

def filter[T](xs: List[T])(p: T => Boolean): List[T] = xs match {
  case Nil => Nil
  case h :: t => if (p(h)) h :: filter(t)(p) else filter(t)(p)
}

def squareListMap(xs: List[Int]): List[Int] = xs map (x => x * x)

def squareListPat(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case h :: t => h * h :: squareListPat(t)
}

def scaleList(xs: List[Double], factor: Double) =
  xs map (x => x * factor)

def map[T, U](xs: List[T])(f: T => U): List[U] = xs match {
  case Nil => Nil
  case h :: t => f(h) :: map(t)(f)
}