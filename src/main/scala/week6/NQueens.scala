package week6

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

  // 与えられたカラムに新しいクイーンを置けるかどうかの判定をする
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    // (row - 1 to 0 by -1)でRangeを作成され、rowが0の場合は空のRangeを作成する
    // 上記のような空のRangeの場合、queensWithRowには空のVectorが入る
    // "queensWithRow zip x" は "Vector(size = 0なので要素は何もない)" になる、
    // なのでforallは実行されずにスキップされ、trueが返される
    // queensが空の場合もVector(size = 0)になる
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    // queensWithRowが空のコレクションの場合、trueが返される
    // カラム同士の減算が行同士の減算と等しい場合、対角線状で一致する
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def main(args: Array[String]): Unit = {
    val queens = List[Int]()
    val row = 2
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    val a = 1 :: Nil

    val result = NQueens.queens(4)
    println(result)
    println(result.size)
  }
}
