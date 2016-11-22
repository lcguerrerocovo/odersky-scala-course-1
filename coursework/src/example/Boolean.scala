package example

/**
  * Created by luisguerrero on 11/4/16.
  */
abstract class Boolean {

  def ifThenElse[T] (t: => T, e: => T) : T

  def && (x: => Boolean): Boolean = ifThenElse[Boolean](x,False)

  def || (x: => Boolean): Boolean = ifThenElse[Boolean](True,x)

  def unary_! : Boolean = ifThenElse[Boolean](False,True)

  def == (x: Boolean): Boolean = ifThenElse[Boolean](x.unary_!,x)

  def != (x: Boolean): Boolean = ifThenElse[Boolean](x,x.unary_!)

  def < (x: Boolean): Boolean =  ifThenElse[Boolean](False,x)
}

object True extends Boolean {

  def ifThenElse[T] (t: => T, e: => T) = t

  override def toString = "true"
}

object False extends Boolean {

  def ifThenElse[T] (t: => T, e: => T) = e

  override def toString = "false"
}

