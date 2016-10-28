val capitals = Map("US" -> "Washington", "Switzerland" -> "Bern")
capitals("US")
capitals get "Canada"
capitals get "Switzerland"
val capsTotal = capitals withDefaultValue("Unknown")
capsTotal("Canada")
val fruit = List("apple", "pineapple", "banana", "orange", "kiwi", "pear")
fruit sortWith (_.length < _.length)
fruit.sorted
fruit sortWith (_ > _)
fruit groupBy (_.head)


val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

val pe1 = new PolyEnhanced(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val pe2 = new PolyEnhanced(0 -> 3.0, 3 -> 7.0)
pe1 + pe2

val pee1 = new PolyEnhanced2(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val pee2 = new PolyEnhanced2(0 -> 3.0, 3 -> 7.0)
pee1 + pee2


class Poly(val terms: Map[Int, Double]) {
  def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

class PolyEnhanced(val t: Map[Int, Double]) extends Poly(t) {
  override val terms = t withDefaultValue 0.0

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  override def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

  override def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
}

class PolyEnhanced2(val t: Map[Int, Double]) extends Poly(t) {
  override val terms = t withDefaultValue 0.0

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  override def +(other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }
}
