import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
}

class Nil[T] extends List[T] {
  override def isEmpty = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")
}

val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))

list.head

def singleton[T](elem: T) = new Cons(elem, new Nil[T])

val t = singleton(true)
t.head

def nth[T](i: Int, xs: List[T]) : T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (i == 0) xs.head
  else nth(i - 1, xs.tail)
}

nth(2, list)
nth(-2, list)
//nth(8, list)
