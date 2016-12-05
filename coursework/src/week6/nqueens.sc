// return a set of possible solutions for n queens problem
def queens(n: Int) : Set[List[Int]] = {
  def diagonals(tuple: (Int,Int), length: Int): Set[(Int, Int)]= {
    Set() ++ ((tuple._1 until length) zip (tuple._2 until length)) ++
      ((tuple._1 to 0 by -1) zip (tuple._2 until length)) ++
      ((tuple._1 until length) zip (tuple._2 to 0 by -1)) ++
      ((tuple._1 to 0 by -1) zip (tuple._2 to 0 by -1))
  }
  def cols(col: Int, length: Int) = {
    Set() ++ (0 until length) zip (List.fill(length)(col))
  }
  def rows(row: Int, length: Int) = {
    Set() ++ (List.fill(length)(row)) zip (0 until length)
  }
  def placeQueens(k: Int): Set[List[Int]] = {
    if(k == 0) Set(List())
    else
      for {
       queens <- placeQueens(k - 1)
       col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  def isSafe(col: Int, queens: List[Int]) : Boolean = {
    val row = queens.length
    val current = (Set() ++ ((row-1 to 0 by -1) zip queens))
    if (current == Set()) true
    else {
      val all = (current flatMap (xy => diagonals(xy,n))) ++
        (current flatMap (xy => rows(xy._1,n))) ++
        (current flatMap (xy => cols(xy._2,n)))
      if(all contains (row,col)) false else true
    }
  }
  placeQueens(n)
}
def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4) map show
assert(queens(8).size == 92)

