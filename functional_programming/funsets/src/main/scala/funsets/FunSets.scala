package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet =
    def f(a:Int):Boolean =
      a==elem
    f

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet =
    def f(a: Int): Boolean =
      contains(s,a) || contains(t,a)
    f
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet =
    def f(a: Int): Boolean =
      contains(s, a) && contains(t, a)
    f

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet =
    def f(a: Int): Boolean =
      contains(s, a) && !contains(t, a)
    f

  /**
   * Returns the subset of `s` for which `p` holds.
   */

  def filter(s: FunSet, p: Int => Boolean): FunSet =
    def f(a:Int): Boolean =
      contains(s, a) && p(a)
    f

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = -1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    def iter(a: Int): Boolean =
      if a > bound then true
      else if s(a) && !p(a) then false
      else iter(a+1)
    iter(bound)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean =
    def f(a:Int):Boolean =
      if a > bound then false
      else if s(a) && p(a) then true
      else f(a+1)
    f(-bound)
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */

  def map(s: FunSet, f: Int => Int): FunSet =
    def iter(a: Int,g :Int => Int): Boolean =
      if contains(s,a) then contains(g,a)
    iter(-bound,f)



  /**
   * Displays the contents of a set
   */
  def s(x:Int): Boolean = x>= -1000 && x<=1000
  def p(x: Int): Boolean = x%2 == 0

  val pun:FunSet = x => x%2==0

  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

object FunSets extends FunSets
