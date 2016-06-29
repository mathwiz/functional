recfun.Main.pascal(1,1);
recfun.Main.pascal(1,2);
recfun.Main.pascal(1,3);
recfun.Main.pascal(2,1);
recfun.Main.pascal(2,2);
recfun.Main.pascal(2,3);

val b = ":-)".toList;
val a= ")".toList
recfun.Main.balance(b);

def count(acc: Int, xs: List[Char]) : Int = {
  if (xs.isEmpty || acc < 0) acc
  else if (xs.head.equals(')')) count(acc - 1, xs.tail)
  else if (xs.head.equals('(')) count(acc + 1, xs.tail)
  else count(acc, xs.tail)
}

count(0, b);
count(0, a);
a.head;
a.head.equals(')');

def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}


countChange(4, List(1,2));

