import math.abs


def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def fixedPoint(f: Double => Double)(guess : Double) = {
  val tolerance = 0.00001
  def closeEnough(x: Double, y: Double) = {
    abs((x - y) / x) / x < tolerance
  }
  def iterate(guess: Double) : Double = {
    val next = f(guess)
    if(closeEnough(guess,next)) next
    else iterate(next)
  }
  iterate(guess)
}


fixedPoint(x => 1 + x/2)(1)

def sqrt(x: Double) = {
  fixedPoint(averageDamp(y => x / y))(1)
}

sqrt(2)