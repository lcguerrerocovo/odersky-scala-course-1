package example

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString = "[" + head + " , " + tail.toString + "]"
}