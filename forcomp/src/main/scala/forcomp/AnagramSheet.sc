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
val fullSet = wordOccurrences("abba")
val allPairs =
  for {
    pair <- fullSet
    i <- 1 to pair._2
  } yield (pair._1, i)
val subsets = fullSet.toSet.subsets.map(x => x.toList).toList
fullSet.indexOf(('b', 2))
val withMoreThanOne = fullSet.filter(x => x._2 > 1)
val withOnlyOne = fullSet.filterNot(x => x._2 > 1)
val expanded = (for {
  pair <- withMoreThanOne
  subset <- subsets if subset.contains(pair)
  i <- 1 to pair._2
} yield subset.patch(subset.indexOf(pair), List((pair._1, i)), 1)).distinct
val all = expanded ++ withOnlyOne

val sub = subset(fullSet)
sub.length - all.length
def subset(occurrences: Occurrences): List[List[(Char, Int)]] = {
  val subsets = occurrences.toSet.subsets.map(x => x.toList).toList
  (for {
    pair <- occurrences
    subset <- subsets if subset.contains(pair)
    i <- 1 to pair._2
  } yield subset.patch(subset.indexOf(pair), List((pair._1, i)), 1)).distinct
}

def wordOccurrences(w: Word): Occurrences = {
  val charCounts = w groupBy (x => x.toLower) map { case (k, v) => (k, v.length) }
  charCounts.toList.sortBy(x => x._1)
}
