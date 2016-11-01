val letters = "ThisisAstring".toList
val lmap = letters.groupBy(x => x.toLower) map { case (k, v) => (k, v.length) }
lmap.toList.sortBy(x => x._1)
