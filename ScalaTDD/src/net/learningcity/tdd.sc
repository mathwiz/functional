val a = List('a', 'b', 'c', 'd')
a.head

def listdown(n: Int, ns: List[Int]): List[Int] = {
  if (n == 0) ns
  else listdown(n - 1, n :: ns)
}

val ns = listdown(4, List())
ns.tail