(1 to 5) flatMap (x => ((6 to 10) map (y => (x,y))))

// dot product
def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  ((xs zip ys).map(xy => xy._1 * xy._2)).sum
}
scalarProduct(List(1.0,2.0),List(1.0,3.0))

def isPrime(x: Int): Boolean =
  (2 to x-1 map (y => x % y)) forall(z => z!=0)


