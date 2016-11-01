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
val fullSet = wordOccurrences("peel")
val allPairs =
  for {
    pair <- fullSet
    i <- 1 to pair._2
  } yield (pair._1, i)
val sub = subset(fullSet)

def subset(occurrences: Occurrences): List[List[(Char, Int)]] = {
  val allPairs =
    for {
      pair <- occurrences
      i <- 1 to pair._2
    } yield (pair._1, i)
  allPairs.toSet.subsets.map(x => x.toList).toList
}

def wordOccurrences(w: Word): Occurrences = {
  val charCounts = w groupBy (x => x.toLower) map { case (k, v) => (k, v.length) }
  charCounts.toList.sortBy(x => x._1)
}
