package example

class NonEmpty(elem : Int, left : IntSet, right : IntSet) extends IntSet {

  def contains(x : Int) = {
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true
  }

  def incl(x : Int) = {
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet) : IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left.toString + elem + right.toString + "}"

}
