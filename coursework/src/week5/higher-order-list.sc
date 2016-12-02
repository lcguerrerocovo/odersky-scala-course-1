
def squareList(xs: List[Int]): List[Int] = xs match {
  case List() => xs
  case y :: ys => y * y :: squareList(ys)
}

def squareListMap(xs: List[Int]) = xs map (x => x*x)

val list = List(1,4,2,6,3,9,-1,0,-10)

squareList(list) == squareListMap(List(1,4,2,6,3,9,-1,0,-10))

list filter (x => x > 0)
list filter (x => x <= 0)
list partition (x => x > 0)
list takeWhile (x => x > 0)
list dropWhile (x => x > 0)
list span (x => x > 0)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys => {
    val lst = xs span (x => x == y)
    lst._1 :: pack(lst._2)
  }
}

def encode[T](xs: List[T]): List[(T,Int)] =
  pack(xs) map (x => (x.head,x.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))