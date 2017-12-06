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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], sumOpen: Int): Boolean = {
      if (chars.isEmpty) sumOpen == 0
      else {
        val n =
          if (chars.head == '(') sumOpen + 1
          else if (chars.head == ')') sumOpen - 1
          else sumOpen
        if (n >= 0) loop(chars.tail, n)
        else false
      }
    }
    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int], count: Int): Int = {
      if (coins.isEmpty) count
      else if (money == 0) 1 + count
      else if (coins.length == 1 && money % coins.head == 0) 1 + count
      else if (coins.head > money) loop(money, coins.tail, count)
      else loop(money, coins.tail, count + loop(money - coins.head, coins, 0))
    }
    loop(money, coins, 0)
  }
}
