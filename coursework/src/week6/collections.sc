(1 to 5) flatMap (x => ((6 to 10) map (y => (x,y))))

// dot product
def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  ((xs zip ys).map(xy => xy._1 * xy._2)).sum
}

scalarProduct(List(1.0,2.0),List(1.0,3.0))

def isPrime(x: Int): Boolean =
  (2 to x-1 map (y => x % y)) forall(z => z!=0)

val n = 7
(1 until n) flatMap (x =>
  (1 until x) map (y => (x,y))) filter (pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i,j)

def dotProduct(xs: List[Double], ys: List[Double]): Double = {
  (for {
    xy <- (xs zip ys)
  } yield xy._1 * xy._2).sum
}

dotProduct(List(1.0,2.0),List(1.0,3.0))