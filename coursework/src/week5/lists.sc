def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs,ys)
}

// O(n*n)
def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAtComplex[T](n: Int, xs: List[T]): List[T] = {
  if(n < 0) throw new Error("index less than 0")
  else if(xs.isEmpty) xs
  else if(n == 0) xs.tail
  else xs.head :: removeAt(n-1,xs.tail)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => Nil
  case List(y) :: zs => y :: flatten(zs)
  case (y :: ys) :: zs => y :: flatten(ys) ::: flatten(zs)
  case y :: ys => y :: flatten(ys)
}
flatten(List(2, List(3)))
flatten(List(List(1, 1), 2, List(3, List(5, 8))))
val l1 = init(List(0,0,1,0,1,0,1))
val l2 = init(List(0))
concat(l1,l1)
reverse(l1)
removeAtComplex(2,l1) == removeAt(2,l1)
removeAtComplex(0,l1) == removeAt(0,l1)

