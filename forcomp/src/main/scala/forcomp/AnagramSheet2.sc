import forcomp.Anagrams._

val sentence = List("Yesman")
val sOcc = sentenceOccurrences(sentence)
val combos = combinations(sOcc)
lengthOfCombo(sOcc)
val combosMap = combos.groupBy(lengthOfCombo).filter(p => p._1 != 0)
val subsets = (for (x <- combosMap.keys.toSet.subsets()) yield x) toList
val sumSubsets = subsets.filter(it => it.foldLeft(0)((a, b) => a + b) == 4)

def lengthOfCombo(occ: Occurrences): Int = occ.foldRight(0) { (x, y) => x._2 + y }

//val anagrams = test(List("a", "bed"))
val anagrams = test(sentence)

def test(sentence: Sentence): List[Sentence] = {
  def iter(occurrences: Occurrences, acc: Sentence): List[Sentence] = occurrences match {
    case Nil => List(acc)
    case occ =>
      for {
        subset <- combinations(occurrences) if dictionaryByOccurrences contains subset
        word <- dictionaryByOccurrences(subset)
        sentence <- iter(subtract(occ, subset), word :: acc)
      } yield sentence
  }
  iter(sentenceOccurrences(sentence), Nil)
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
