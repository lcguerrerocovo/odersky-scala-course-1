package example

class Succ(val n: Nat) extends Nat {

  def isZero: Boolean = False

  def predecessor: Nat = n

  def + (that: Nat): Nat = {
    def iter(pre: Nat,sum: Nat) : Nat =
      pre.isZero.ifThenElse(sum,iter(pre.predecessor, new Succ(sum)))
    iter(that,this)
  }

  def - (that: Nat): Nat = {
    def iter(pre: Nat,sub: Nat) : Nat =
      pre.isZero.ifThenElse(sub,iter(pre.predecessor,sub.predecessor))
    iter(that,this)
  }

  override def toString = "(Succ" + n.toString + ")"
}