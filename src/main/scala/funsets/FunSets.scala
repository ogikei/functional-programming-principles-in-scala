package funsets

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    (x: Int) => x == elem
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    (x: Int) => s(x) || t(x)
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    (x: Int) => s(x) && t(x)
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    (x: Int) => s(x) && !t(x)
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    (x: Int) => s(x) && p(x)
  }

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  /**
    * Set sがa(-1000 ~ 1000)を含んでいて、条件pを満たす場合は再びイテレートされる
    * これにより、Setの要素全てが条件を満たしている場合はtrueを返すようにしている
    *
    * @param s
    * @param p
    * @return
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a - 1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   *
   */
  /**
    * p(x)をそのまま渡した場合、Setに含まれているすべての要素が真でないとtrueを返さないが、
    * !p(x)にすることでaが含まれていて、p(x)を満たす場合はfalseを返すようになる
    *
    * @param s
    * @param p
    * @return
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    !forall(s, (x: Int) => !p(x))
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  /**
    * xにはmapで処理された後の値が入る
    * 例えば、fがx => x * xなら、帰ってきたSetに25を渡せばtrueが返ってくる
    *
    * @param s
    * @param f
    * @return
    */
  def map(s: Set, f: Int => Int): Set = {
    (x: Int) => exists(s, (a: Int) => f(a) == x)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
