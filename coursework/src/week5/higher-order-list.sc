
def scaleList(xs: List[Int], factor: Int): List[Int] = xs match {
  case List() => xs
  case y :: ys => y * factor :: scaleList(ys,factor)
}

scaleList(List(1,4,2,6,3,9,-1,0,-10),2)
