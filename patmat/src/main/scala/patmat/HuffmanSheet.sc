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

val chars = List('z', 'c', 'a', 'a', 'b', 'c', 'a', 'd', 'd', 'b', 'y', 'z', 'a', 'e', 'e', 'e', 'd')
val t = times(chars)

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
  def order(leaf: Leaf, leafs: List[Leaf]): List[Leaf] = leafs match {
    case List() => leaf :: Nil
    case h1 :: t =>
      if (leaf.weight > h1.weight) h1 :: order(leaf, t)
      else leaf :: h1 :: t
  }
  def iter(xs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] = xs match {
    case List() => acc
    case h :: t => iter(t, order(Leaf(h._1, h._2), acc))
  }
  iter(freqs, Nil)
}



makeOrderedLeafList(t)

