abstract class Function1[A, B] {
  def apply(a: A): B
}

val f = {
  class AnonFun extends Function1[Int, Int] {
    def apply(x: Int) = x * x
  }
  new AnonFun
}

f.apply(4)

val g = new Function1[Int, Int] {
  override def apply(a: Int): Int = a * a
}

g.apply(4)

