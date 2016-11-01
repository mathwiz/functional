import scala.io.Source

object x {
  val in = Source.fromFile("C:\\Users\\Yohan\\Documents\\GitHub\\TDD\\python\\intro_comp_sci_600\\words.txt")
  val words = in.getLines.toList filter (word => word forall (ch => ch.isLetter))
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
  val charCode: Map[Char, Char] = for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

  def wordCode(word: String): String = word.toUpperCase map charCode

  wordCode("food")
  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty)
      Set(Nil)
    else
      (for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest).toSet

  encode("7225247386")

  def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

  val solutions = translate("7225247386")
  for {phrase <- solutions} println(phrase)
}