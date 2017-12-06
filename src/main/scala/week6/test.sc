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
  // ここで行うことは次の通り
  // 1. 最初は下記の通り、全ての場合が真となる
  //    これは各カラムの0行目から見ていくことを表している
  //    すなわち、(0, 0), (0, 1), (0, 2), (0, 3)...(0, n) から各々クイーンを埋めていき、
  //    条件を満たすものだけを最終的にアウトプットする
  //    => Set(List(0), List(1), List(2), List(3))
  // 2. 次は行の2番目に入るクイーンも含めてSetで表す
  //    rowにはクイーンを入れようとしている列が入るので、ここでは1となる
  //    queensWithRowでrowとcolのペアをzipで作成する
  //    => Vector((0,0)), Vector((0,1)), Vector((1,3), (0,1)) etc.
  // 3. 上記ペアをforallで条件を満たすか判定する
  //    =>
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    // (row - 1 to 0 by -1) => Range
    // rowが0の場合は空のRangeを作成する => Vector()
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    // もしqueensWithRowが空のコレクションの場合、trueが返される
    // カラムのインデックスが同じでない場合 && カラム同士の減算が行同士の減算と等しい場合、対角線状で一致する
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }
}

val result = NQueens.queens(4)
