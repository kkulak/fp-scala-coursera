package recfun
import common._

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

    def isLeftBoundary(c: Int, r: => Int) =
      c == 0

    def isRightBoundary(c: Int, r: Int) =
      c == r

    def isBoundary(c: Int, r: Int) =
      isLeftBoundary(c, r) || isRightBoundary(c, r)

    def finder(c: Int, r: Int): Int = {
      if (isBoundary(c, r)) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

    finder(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isInvalid(openingBraces: Int, chars: List[Char]) = {
      openingBraces < 0 || chars.isEmpty && openingBraces != 0
    }

    def isEnd(openingBraces: Int, chars: List[Char]) = {
      openingBraces == 0 && chars.isEmpty
    }

    def isOpening(c: Char) = c == '('

    def isClosing(c: Char) = c == ')'

    def finder(openingBraces: Int, chars: List[Char]): Boolean = {
      if(isInvalid(openingBraces, chars)) false
      else if(isEnd(openingBraces, chars)) true
      else if(isOpening(chars.head)) finder(openingBraces + 1, chars.tail)
      else if (isClosing(chars.head)) finder(openingBraces - 1, chars.tail)
      else finder(openingBraces, chars.tail)
    }

    finder(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if( money == 0 ) 1
    else if( money < 0 || coins.isEmpty ) 0
    else countChange( money - coins.head, coins ) + countChange( money , coins.tail )
  }

}
