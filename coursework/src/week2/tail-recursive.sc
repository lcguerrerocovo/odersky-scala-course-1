def sum(f: Int => Int,a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x * x , 3,4)

def product(f: Int => Int) : (Int, Int) => Int = {
  def productF(a: Int, b: Int) : Int = {
    if (a > b) 1
    else f(a) * productF(a+1, b)
  }
  productF
}


def general(f: Int => Int, operation: (Int, Int) => Int, base : Int)(a: Int, b: Int) : Int = {
    if (a > b) base
    else operation(f(a),general(f,operation,base)(a+1, b))
}

def factorial(a: Int): Int = {
  general(x => x,(a,b) => a*b,1)(1,a)
}

factorial(4)