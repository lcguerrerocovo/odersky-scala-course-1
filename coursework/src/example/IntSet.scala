package example

abstract class IntSet {
  def contains(x : Int) : Boolean
  def incl(x : Int) : IntSet
  def union(other : IntSet) : IntSet
}
