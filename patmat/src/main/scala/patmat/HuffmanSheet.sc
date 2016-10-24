import patmat.Huffman._

val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
)
val sampleTree2 = makeCodeTree(Leaf('t', 2), Leaf('x',1))

val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
val chars = List('z', 'c', 'a', 'a', 'b', 'c', 'a', 'd', 'd', 'b', 'y', 'z', 'a', 'e', 'e', 'e', 'd')
val t = times(chars)
val ts = makeOrderedLeafList(t)
val comb = combine(ts)
val s = singleton(comb)
val huffman = until(singleton, combine)(ts)
val encoded = encode(huffman.head)("cabbyebyebyebeedeedbabybabyzedeyeaddeye".toList)
val decoded = decode(huffman.head, encoded)
val charsTest = extractChars(huffman.head)
val codeTable = convert(huffman.head)
val quick = quickEncode(huffman.head)("cabbyebyebyebeedeedbabybabyzedeyeaddeye".toList)
val quickDecode = decode(huffman.head, quick)
val worked = decoded.equals(quickDecode)

def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val table = convert(tree)
  def accumulate(cs: List[Char], acc: List[Bit]): List[Bit] = cs match {
    case Nil => acc
    case h::t => accumulate(t, codeBits(table)(h) ::: acc)
  }
  accumulate(text, Nil).reverse
}

def convert(tree: CodeTree): CodeTable = {
  val chars = extractChars(tree)
  def encodeChar(sub: CodeTree, ch: Char, acc: List[Bit]): List[Bit] = sub match {
    case Leaf(c, w) => if (c==ch) acc else Nil
    case Fork(l, r, cs, w) => if (cs.contains(ch)) encodeChar(l, ch, 0::acc) ::: encodeChar(r, ch, 1::acc) else Nil
  }
  def accumulate(cs: List[Char], acc: CodeTable): CodeTable = cs match {
    case Nil => acc
    case h::t => accumulate(t, mergeCodeTables(List((h, encodeChar(tree, h, Nil))), acc))
  }
  accumulate(chars, Nil)
}

def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

def extractChars(tree: CodeTree): List[Char] = {
  def walk(t: CodeTree, acc: List[Char]) : List[Char] = t match {
    case Leaf(c, w) => c :: acc
    case Fork(l, r, cs, w) => walk(l, acc) ::: walk(r, acc)
  }
  walk(tree, Nil)
}

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def decodeChar(sub: CodeTree, bs: List[Bit], acc: Int): (Char, Int) = sub match {
    case Leaf(c, w) => (c, acc)
    case Fork(l, r, cs, w) => decodeChar(if (bs.head == 0) l else r, bs.tail, acc+1)
  }
  def accumulate(bs: List[Bit], acc: List[Char]): List[Char] = bs match {
    case Nil => acc
    case h::t =>
      val decode = decodeChar(tree, bs, 0)
      accumulate(bs.drop(decode._2), decode._1::acc)
  }
  accumulate(bits, Nil).reverse
}

def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def encodeChar(sub: CodeTree, ch: Char, acc: List[Bit]): List[Bit] = sub match {
    case Leaf(c, w) => if (c==ch) acc else Nil
    case Fork(l, r, cs, w) => if (cs.contains(ch)) encodeChar(l, ch, 0::acc) ::: encodeChar(r, ch, 1::acc) else Nil
  }
  def accumulate(cs: List[Char], acc: List[Bit]): List[Bit] = cs match {
    case Nil => acc
    case h::t => accumulate(t, encodeChar(tree, h, Nil) ::: acc)
  }
  accumulate(text, Nil).reverse
}

def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
  if (singleton(trees)) trees
  else until(singleton, combine)(combine(trees))

def combine(trees: List[CodeTree]): List[CodeTree] = {
  def order(f: Fork, xs: List[CodeTree]) : List[CodeTree] = xs match {
    case List() => f :: Nil
    case h :: t =>
      if (f.weight > weight(h)) h :: order(f, t)
      else f :: xs
  }
  trees match {
    case List() => List()
    case h :: Nil => trees
    case h1 :: h2 :: t => order(makeCodeTree(h1, h2), t)
  }
}

def singleton(trees: List[CodeTree]): Boolean = return trees.length == 1

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

def times(chars: List[Char]): List[(Char, Int)] = {
  def iter(acc: List[(Char, Int)], cs: List[Char]): List[(Char, Int)] = {
    if (cs.isEmpty) acc
    else if (!acc.isEmpty && acc.head._1 == cs.head) iter((acc.head._1, acc.head._2 + 1) :: acc.tail, cs.tail)
    else iter((cs.head, 1) :: acc, cs.tail)
  }
  iter(List(), chars.sorted)
}