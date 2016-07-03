type Set = Int => Boolean
val a = Set(0) + 2 + 4 + 6
singletonSet(3)
singletonSet(-1)
contains(a, 2)
contains(a, 3)
contains(singletonSet(3), 3)
val b = Set(0) + 1 + 3 + 5
toString(union(a, b))
toString(intersect(a, b))
toString(diff(a, b))
toString(filter(a, x => x > 2))

val bound = 1000
def toString(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}
def contains(s: Set, elem: Int): Boolean = s(elem)
def singletonSet(elem: Int): Set = Set(elem)
def union(s: Set, t: Set): Set = x => contains(s, x) || contains(t, x)
def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)
def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)
def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)