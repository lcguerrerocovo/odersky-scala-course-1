

abstract class IntSet {
  def contains(x : Int) : Boolean
  def incl(x : Int) : IntSet
  def union(other : IntSet) : IntSet
}

class Empty extends IntSet {
  def contains(x : Int) = false
  def incl(x : Int) = new NonEmpty(x, new Empty, new Empty)
  def union(other : IntSet) = other
  override def toString = "."
}

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

val t1 = new NonEmpty(2, new Empty, new Empty)
val t2 = new NonEmpty(3, new Empty, new Empty)
val t3 = t1 incl 4

t2 union t3

def f(xs: List[NonEmpty], x: Empty) = xs prepend x

