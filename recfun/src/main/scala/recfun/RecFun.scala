package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || r == 0 || c == r) 1
    else (pascal(c-1, r-1) + pascal(c, r-1))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def inner_balance(chars: List[Char], unbcount: Int): Boolean = {
      if( chars.isEmpty ) unbcount == 0
      else if( unbcount < 0) false
      else{
        if( chars.head == '(' ) inner_balance(chars.tail, unbcount + 1)
        else if( chars.head == ')' ) inner_balance( chars.tail, unbcount - 1)
        else inner_balance(chars.tail, unbcount)
      }
    }

    inner_balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def inner_countChange(money: Int, coins: List[Int]): Int ={
      if( money == 0 ) 1
      else if( money <= 0 || coins.isEmpty ) 0
      else inner_countChange( money - coins.head, coins) + inner_countChange( money, coins.tail)
    }

    if( money <= 0 ) 0
    else inner_countChange(money, coins.sorted.reverse)
  }
}
