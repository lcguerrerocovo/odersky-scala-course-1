
def msort[T](xs: List[T])(lt: (T,T) => Boolean): List[T] = {
  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs,ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xss, y :: yss) => {
        if(lt(x,y)) y :: merge(xs,yss)
        else x :: merge(xss,ys)
      }
    }
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst,snd) = xs splitAt n
    merge(msort(fst)(lt),msort(snd)(lt))
  }
}

msort(List(1,4,2,6,3,9,-1,0,-10))((x: Int, y: Int) => x > y)
msort(List("apple","acai","acerola","banana"))((x: String, y: String) => x.compareTo(y) > 0)