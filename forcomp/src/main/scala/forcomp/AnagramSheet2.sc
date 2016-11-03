import forcomp.Anagrams._

val sOcc = sentenceOccurrences(List("A", "bed"))
val combos = combinations(sOcc)
val a1 = wordAnagrams("A")
val a2 = wordAnagrams("bed")
val a3 = wordAnagrams("ad")
val combosMap = combos.groupBy(x => x.length).filter(p => p._1 != 0)
val subsets = (for (x <- combosMap.keys.toSet.subsets()) yield x) toList
val sumSubsets = subsets.filter(p => p.foldLeft(0)((a, b) => a + b) == 4)
Set(1, 2, 3).foldLeft(0)((a, b) => a + b)
