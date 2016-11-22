package example


abstract class Nat {

  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def + (that: Nat): Nat

  def - (that: Nat): Nat
}

object Zero extends Nat {

  def isZero: Boolean = True

  def predecessor: Nat = {
    throw new Exception("Natural: Zero has no predecessor")
  }

  def + (that: Nat): Nat = that

  def - (that: Nat): Nat = that.isZero.ifThenElse(this,
    throw new Exception("Natural: Zero has no predecessor"))

  override def toString = "(Zero)"
}
