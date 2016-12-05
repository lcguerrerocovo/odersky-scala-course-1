def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x,y) => f(x) :: y)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x,y) => (y+1))

lengthFun(List("a", "a", "a", "b", "c", "c", "a"))

mapFun[Int,Int](List(1, 2, 3, 4, 5, 6, 7),x => (x + x))

mapFun[String,Int](List("a", "aa", "aaa", "bbbb", "c", "cc", "aaa"),x => x.length)
