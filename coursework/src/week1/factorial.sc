def factorial(x: Int): Int = {
  if(x <= 1) 1 else x * factorial(x - 1)
}
factorial(0)
factorial(4)
def factIter(x: Int): Int = {
  def iter(y: Int, count: Int): Int = {
    if(count == 0) 1
    else if (count == 1) y
    else iter(y *  (count - 1), count - 1)
  }
  iter(x,x)
}
factIter(0)
