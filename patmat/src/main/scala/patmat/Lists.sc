package patmat

object Test {
  object List {
    def apply[T](a: T, b: T): List[T] = new Cons(a, new Cons(b, new Nil))

    def apply[T](a: T): List[T] = new Cons(a, new Nil)

    def apply[T]() = new Nil
  }

  trait List[T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }

  class Nil[T] extends List[T] {
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException

    override def tail: Nothing = throw new NoSuchElementException
  }

  val x = List()

  val y = List(1)

  val z = List(3, 5)
}

