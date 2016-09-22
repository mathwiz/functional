import patmat.Huffman
import patmat.Huffman.{Fork, Leaf}

val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
val sampleTree = Huffman.makeCodeTree(
  Huffman.makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
)

def times(chars: List[Char]): List[(Char, Int)] = {
  def iter(acc: List[(Char, Int)], cs: List[Char]): List[(Char, Int)] = {
    if (cs.isEmpty) acc
    else if (!acc.isEmpty && acc.head._1 == cs.head) iter((acc.head._1, acc.head._2 + 1) :: acc.tail, cs.tail)
    else iter((cs.head, 1) :: acc, cs.tail)
  }
  iter(List(), chars.sorted)
}

val chars = List('z', 'c', 'a', 'a', 'b', 'c', 'a', 'd', 'd')
chars.sorted
val t = times(chars)
t
Huffman.chars(t2)





