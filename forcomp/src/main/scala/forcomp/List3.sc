val a = List(1, 2, 3, 4, 5, 6)
val b = List(10, 20, 30, 40, 50, 60)
val c = List(List(1, 1), 2, List(3, List(5, 8)))
concat(a, b)
reverse(a)
removeAt(2, a)
flatten(c)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => Nil
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = {
  xs match {
    case List() => List()
    case y :: ys => if (n==0) removeAt(n-1, ys) else y :: removeAt(n-1, ys)
  }
}

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (h:List[_]) :: t => flatten(h) ::: flatten(t)
  case h :: t => h :: flatten(t)
}