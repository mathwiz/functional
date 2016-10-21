val fruit = List("apples", "oranges", "pears")
val veg = Nil.::("kale").::("broccoli").::("spinach")
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val empty = List()
val nums = List().::(4).::(3).::(2).::(1)
val nums2 = 1 :: 2 :: 3 :: 4 :: Nil
val nums3 = Nil.::(4).::(3).::(2).::(1)
val nums4 = List(3, 4, 1, 6, 1, 0)
nums.tail
fruit.head
veg.head
isort(nums4)

val c = List(nums, 88, nums4, 77)
flatten(c)

def flatten(xs: List[Any]): List[Any] =
  xs match {
    case List() => List()
    case y :: ys => ys
  }

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => x :: Nil
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}



