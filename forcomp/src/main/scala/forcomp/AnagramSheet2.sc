import forcomp.Anagrams._

val sentence = (List("A", "bed"))
val sOcc = sentenceOccurrences(sentence)
val combos = combinations(sOcc)
wordAnagrams("abed")
wordAnagrams("rules")
lengthOfCombo(sOcc)
val combosMap = combos.groupBy(lengthOfCombo).filter(p => p._1 != 0)
val subsets = (for (x <- combosMap.keys.toSet.subsets()) yield x) toList
val sumSubsets = subsets.filter(it => it.foldLeft(0)((a, b) => a + b) == 4)

def lengthOfCombo(occ: Occurrences): Int = occ.foldRight(0) { (x, y) => x._2 + y }

val anagrams = test(sentence)

def test(sentence: Sentence): List[Sentence] = {
  def perm(occurrences: Occurrences, acc: Sentence): List[Sentence] = occurrences match {
    case Nil => List(acc)
    case occ =>
      for {
        subset <- combinations(occurrences)
        word <- dictionaryByOccurrences(subset)
        sentence <- perm(subtract(occ, subset), acc ::: List(word))
      } yield sentence
  }
  perm(sentenceOccurrences(sentence), Nil)
}


for (lengths <- sumSubsets; length <- lengths) yield length
val goodOccs = occurrencesOfLength(List(1, 3))

def occurrencesOfLength(lengths: Seq[Int]): Seq[List[Occurrences]] = {
  for (l <- lengths) yield combosMap.getOrElse(l, Nil)
}
//find all possible 3 letter words with 1 letter word
val wordCombos = combinationList(List(List('a', 'b', 'e'), List('d')))
def combinationList[T](ls: List[List[T]]): List[List[T]] = ls match {
  case Nil => Nil :: Nil
  case head :: tail => val rec = combinationList[T](tail)
    rec.flatMap(r => head.map(t => t :: r))
}
