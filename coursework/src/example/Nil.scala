package example

class Nil[T] extends List[T] {
  def isEmpty = true
  def head : Nothing = throw new NoSuchElementException("Nil.head")
  def tail : Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString = "[ Nil ]"
}
