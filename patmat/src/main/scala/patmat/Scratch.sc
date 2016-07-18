abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  /* Violation of simple Peano numbers for testing */
  def intVal: Int
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new UnsupportedOperationException("no predecessor")

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new UnsupportedOperationException("subtraction produces negative")

  override def toString: String = "0"

  override def intVal: Int = 0
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  override def toString: String = "Succ(" + n + ")"

  override def intVal: Int = n.intVal + 1
}

val one = new Succ(Zero)
val two = new Succ(one)
val four = two + two
four.intVal

