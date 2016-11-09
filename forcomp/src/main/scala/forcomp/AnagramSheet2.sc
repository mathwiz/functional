import forcomp.Anagrams._

val sOcc = sentenceOccurrences(List("A", "bed"))
val combos = combinations(sOcc)
val a2 = wordAnagrams("abed")
val a3 = wordAnagrams("ad")
val combosMap = combos.groupBy(x => x.length).filter(p => p._1 != 0)
val subsets = (for (x <- combosMap.keys.toSet.subsets()) yield x) toList
val sumSubsets = subsets.filter(it => it.foldLeft(0)((a, b) => a + b) == 4)
for (lengths <- sumSubsets; length <- lengths) yield length
val goodOccs = occurrencesOfLength(List(1,3))

def occurrencesOfLength(lengths: Seq[Int]): Seq[List[Occurrences]] = {
  for (l <- lengths) yield combosMap.getOrElse(l, Nil)
}
//find all possible 3 letter words with 1 letter word
val wordCombos = combinationList(List(List('a','b','e'), List('d')))
def combinationList[T](ls: List[List[T]]): List[List[T]] = ls match {
  case Nil => Nil :: Nil
  case head :: tail => val rec = combinationList[T](tail)
    rec.flatMap(r => head.map(t => t :: r))
}
