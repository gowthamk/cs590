package cs590.week1

/**
 * Problem 1: Intro to Scala
 *
 * (largely taken from "Functional Programming Principles in Scala"
 * tought at EPFL and on Coursera)
 */

trait FunSets {

  /**
   * This type alias defines how sets are represented.
   */
  type Set = Int => Boolean

  /**
   * This function tests whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * This function displays the contents of a set.
   */
  def toString(s: Set): String = {
    val xs = for (i <- -1000 to 1000 if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * TODO: construct a single-element Set
   */
  def set(elem: Int): Set = (i:Int) => i==elem

  def emptySet = (i:Int) => false

  /**
   * TODO: implement union, intersection, difference, etc
   */
  def union(s: Set, t: Set): Set = (i:Int) => s(i) || t(i)

  def intersect(s: Set, t: Set): Set = (i:Int) => s(i) && t(i)

  def diff(s: Set, t: Set): Set = (i:Int) => s(i) && !(t(i))

  def filter(s: Set, p: Int => Boolean): Set = (i:Int) => s(i) && p(i)

  def forall(s: Set, p: Int => Boolean): Boolean = {
    val xs = for (i <- -1000 to 1000 if contains(s,i)) yield i
    xs.forall(p)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    val xs = for (i <- -1000 to 1000 if contains(s,i)) yield i
    xs.exists(p)
  }

  def map(s: Set, f: Int => Int): Set = {
    val xs = for (i <- -1000 to 1000 if contains(s,i)) yield f(i)
    xs.foldLeft[Set] (emptySet) ((s,i) => union(s,set(i)))
  }

}


trait IntSets {

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
      
    /**
     * TODO: implement missing functionality (Bonus)
     */

    // def intersect(that: IntSet): IntSet
    // def intersect0(that: IntSet, accu: IntSet): IntSet

    // def filter(p: Int => Boolean): IntSet

    // def intersect2(that: IntSet): IntSet
  }

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    override def toString() = "Empty"
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this
    
    override def toString() = "NonEmpty(%d, %s, %s)".format(elem, left, right)
  }

}


