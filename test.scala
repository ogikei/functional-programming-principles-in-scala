object NQueens {

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if (isSafe(col, queens))
        } yield col :: queens
      }
    }

    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    // (row - 1 to 0 by -1)でRangeを作成され、rowが0の場合は空のRangeを作成する
    // 上記のような空のRangeの場合、queensWithRowには空のVectorが入る
    // queensWithRow zip xはVector(size = 0なので要素は何もない)になる、
    // なのでforallは実行されずにスキップされ、trueが返される
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    // queensWithRowが空のコレクションの場合、trueが返される
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def main(args: Array[String]): Unit = {
    val result = NQueens.queens(4)
    println(result)
    println(result.size)
  }
}

