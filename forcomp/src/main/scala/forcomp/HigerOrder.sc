val a = List(1, 31, 41, 42, 43, 61)
map(a)(x => 2 * x)
scaleList(map(a)(x => x * 1.0), Math.pow(.5, 3))

def scaleList(xs: List[Double], factor: Double) =
  xs map (x => x * factor)

def map[T, U](xs: List[T])(f: T => U): List[U] = xs match {
  case Nil => Nil
  case h :: t => f(h) :: map(t)(f)
}