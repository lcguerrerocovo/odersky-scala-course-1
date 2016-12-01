def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs,ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xss, y :: yss) => {
      if(x > y) y :: merge(xs,yss)
      else x :: merge(xss,ys)
    }
}

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst,snd) = xs splitAt n
    merge(msort(fst),msort(snd))
  }
}

msort(List(1,4,2,6,3,9,-1,0,-10))