trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

val one = Number(1)
val two = Number(2)

Sum(Sum(Number(1), Number(2)), Number(3)).eval
Number(4).eval

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

eval((Sum(Sum(Number(1), Number(2)), Number(3)))) + eval(Number(4))

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
}

show(Sum(Number(1), Sum(Number(2), Number(3))))
