/**
 * Solving problems from this list http://aperiodic.net/phil/scala/s-99/
 *
 * To run this code, go to test/ListsSuite.scala and run as JUnitTest.
 *
 */

package ninetynineproblems

object Lists {
  /** Start of: P01 */
  def last[T](list: List[T]): T =
    list.last

  def lastRec[T](list: List[T]): T = list match {
    case Nil => throw new Error("Cannot get last element of empty list")
    case h :: Nil => h
    case h :: tail => lastRec(tail)
  }

  /** End of: P01 */

  /** Start of: P02 */

  def penultimate[T](list: List[T]): T = list match {
    case Nil => throw new Error("List is empty")
    case h :: Nil => throw new Error("List contains only one element")
    case h :: sh :: Nil => h
    case l => penultimate(l.tail)
  }

  /** End of: P02 */

  /** Start of: P03 */
  
  def nth[T](n: Int, list: List[T]): T = {
    def rec(remain: List[T], index: Int): T =
      if (index < 0) throw new IndexOutOfBoundsException
      else if (index == 0 && !remain.isEmpty) remain.head
      else if (index > 0 && !remain.isEmpty) rec(remain.tail, index - 1)
      else throw new IndexOutOfBoundsException

    rec(list, n)
  }
  
  /** End of: P03 */
  
  /** Start of: P04 */
  
  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case h :: tail => 1 + length(tail)
  }
    
  /** End of: P04 */
  
  /** Start of: PO5 */
  
  def reverse[T](list: List[T]): List[T] = list match {
    case Nil => list
    case h :: tail => reverse(tail) :+ h
  }
  
  /** End of: P05 */
  
  /** Start of: P06 */
  
  def isPalindrom[T](list: List[T]): Boolean = list match {
    case Nil => true
    case h :: Nil => true
    case h :: tail => if (h == lastRec(tail)) isPalindrom(tail.init) else false
  }
  
  /** End of: P06 */
  
  /** Start of: P07 */
  
  /** End of: P07 */
}