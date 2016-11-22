package example

class Empty extends IntSet {
  def contains(x : Int) = false
  def incl(x : Int) = new NonEmpty(x, new Empty, new Empty)
  def union(other : IntSet) = other
  override def toString = "."
}
