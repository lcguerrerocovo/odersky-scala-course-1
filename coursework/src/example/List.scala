package example

trait List[+T] {
  def isEmpty : scala.Boolean
  def head : T
  def tail : List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object List {

  def apply[T]() : List[T] = new Nil

  def apply[T](a: T) : List[T] =
    new Cons(a,apply())

  def apply[T](a: T, b: T) : List[T] =
    new Cons(b,apply(a))
}

