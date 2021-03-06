package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(" " + pascal(col, row))
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == c || c == 0 || r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def count(acc: Int, xs: List[Char]): Int = {
      if (xs.isEmpty || acc < 0) acc
      else if (xs.head.equals(')')) count(acc - 1, xs.tail)
      else if (xs.head.equals('(')) count(acc + 1, xs.tail)
      else count(acc, xs.tail)
    }

    count(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
