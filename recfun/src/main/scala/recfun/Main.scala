package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 0 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def findMatchingParens(string: List[Char]): List[Char] = {
      if (string.isEmpty) string
      else if (string(1) == '(') {
        findMatchingParens(string.head :: findMatchingParens(string.tail))
      }
      else {
        var newString = string
        newString = newString.drop(1)
        newString = newString.drop(1)
        newString
      }
    }

    def eatNonParensChars(string: List[Char]): List[Char] = {
      if (string.isEmpty) string
      else if (string.head != ')' && string.head != '(') {
        eatNonParensChars(string.tail)
      }
      else string.head :: eatNonParensChars(string.tail)
    }

    var newChars = eatNonParensChars(chars)

    if (newChars.isEmpty) true
    else if (newChars(0) == '(' && newChars.length > 1) {
      newChars = findMatchingParens(newChars)
      balance(newChars)
    }
    else false

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else {
      val coinsSorted = coins.sorted
      var count = 0
      if (money - coinsSorted.head >= 0) {
        var remaining = money
        while (remaining > 0) {
          remaining = remaining - coinsSorted.head
          if(remaining == 0) {
            count += 1
          } else {
            count += countChange(remaining, coinsSorted.tail)
          }
        }
      } else {
        count += countChange(money, coinsSorted.tail)
      }
      count += countChange(money, coinsSorted.tail)
      count
    }
  }
}
