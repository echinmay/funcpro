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
      if ((c < 0) || (c > r)) {
        0
      } else if (c == 0) {
        1
      } else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkacc(chrarr: List[Char], acc: Int): Boolean = {
        if (acc < 0) false
        else if (chrarr.isEmpty) acc==0
        else chrarr.head match {
          case '(' => checkacc(chrarr.tail, acc+1)
          case ')' => checkacc(chrarr.tail, acc-1)
          case _ => checkacc(chrarr.tail, acc)
        }
      }
      checkacc(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else coins match {
        // Count taking the first coin and without the first coin
        case x :: xs1 => countChange((money - x), coins)  + countChange(money, xs1)
        case _ => 0
      }
    }
  }
