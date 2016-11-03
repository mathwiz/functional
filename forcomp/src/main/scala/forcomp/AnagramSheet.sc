type Occurrences = List[(Char, Int)]
type Word = String
type Sentence = List[Word]

val letters = "ThisisAstring"
val lmap = letters.groupBy(x => x.toLower) map { case (k, v) => (k, v.length) }
val occurrences = lmap.toList.sortBy(x => x._1)
val sentence = List("A", "bird", "in", "the", "hand", "is", "worth", "two", "in", "the", "bush")
sentence.mkString("")
val words = List("ate", "eat", "tea", "ape")
words groupBy (x => wordOccurrences(x))
val fullSet = wordOccurrences("belle")
val allPairs =
  for {
    pair <- fullSet
    i <- 1 to pair._2
  } yield (pair._1, i)
val subsets = fullSet.toSet.subsets.map(x => x.toList).toList
val allSubsets = allPairs.toSet.subsets.map(x => x.toList).toList.distinct
val withMoreThanOne = fullSet.filter(x => x._2 > 1)
val withOnlyOne = fullSet.filterNot(x => x._2 > 1)
val expanded = (for {
  pair <- withMoreThanOne
  subset <- subsets if subset.contains(pair)
  i <- 1 to pair._2
} yield subset.patch(subset.indexOf(pair), List((pair._1, i)), 1)).distinct
val smallSubset = List(('a', 2), ('b', 2))
val expanded2 = smallSubset.map(p => for (n <- List.range(1, p._2 + 1)) yield (p._1, n))

val combos = combinations(smallSubset)
val combos1 = combinations(List(('a',2)))
def combinations(pairs: List[(Char, Int)]): List[List[(Char, Int)]] = {
  val allPairs = pairs map (p => for (n <- List.range(1, p._2 + 1)) yield (p._1, n))
  def combinationList(ls: List[List[(Char,Int)]]): List[List[(Char,Int)]] = ls match {
    case Nil => Nil :: Nil
    case head :: tail => val rec = combinationList(tail)
      rec.flatMap(r => head.map(t => t :: r))
  }
  combinationList(allPairs)
}

val combos2 = combinationList(List(List(('a', 1), ('a', 2)), List(('b', 1), ('b', 2))))
def combinationList[T](ls: List[List[T]]): List[List[T]] = ls match {
  case Nil => Nil :: Nil
  case head :: tail => val rec = combinationList[T](tail)
    rec.flatMap(r => head.map(t => t :: r))
}

val sub = subset(fullSet)
sub.length
def subset(occurrences: Occurrences): List[List[(Char, Int)]] = {
  val subsets = occurrences.toSet.subsets.map(x => x.toList).toList
  def combinationList(ls: List[List[(Char,Int)]]): List[List[(Char,Int)]] = ls match {
    case Nil => Nil :: Nil
    case head :: tail => val rec = combinationList(tail)
      rec.flatMap(r => head.map(t => t :: r))
  }
  def combinations(pairs: List[(Char, Int)]): List[List[(Char, Int)]] = {
    val allPairs = pairs map (p => for (n <- List.range(1, p._2 + 1)) yield (p._1, n))
    combinationList(allPairs)
  }
  subsets flatMap combinations
}

def wordOccurrences(w: Word): Occurrences = {
  val charCounts = w groupBy (x => x.toLower) map { case (k, v) => (k, v.length) }
  charCounts.toList.sortBy(x => x._1)
}
