package example

class Rational(x: Int, y: Int) {
  //require( y > 0 ,"denominator must be non zero")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int) : Int = if(b == 0) a else gcd(b, a % b)

  val numer = x
  val denom = y


  def - (subtrahend: Rational): Rational = {
    new Rational(numer*subtrahend.denom - denom*subtrahend.numer,
      subtrahend.denom*denom)
  }

  def unary_- = new Rational(-numer,denom)

  def < (that : Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if(this < that) that else this

  override def toString = numer / (gcd(numer,denom)) + "/" + denom / (gcd(numer,denom))
}
