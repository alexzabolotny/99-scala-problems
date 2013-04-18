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
  
  /** Start of: P07 */
  def flatten(in: List[Any]): List[Any] = in match {
    case h :: tail => (h match {
      case l: List[Any] => flatten(l)
      case i => List(i)
    }) ::: flatten(tail)
    case Nil => List()
    case _ => List(in)
  }
  /** End of: P07 */

  /** Start of: P08 */
  def compress[T](in: List[T]): List[T] = in match {
    case Nil => List()
    case h :: Nil => List(h)
    case h :: ph :: tail => if (h == ph) compress(h :: tail) else h :: compress(ph :: tail)
  }
  /** End of: P08 */

  /** Start of: P09 */
  def pack[T](in: List[T]): List[List[T]] = {
    def rec(in: List[T], accu: List[T]): List[List[T]] = in match {
      case Nil => List(accu)
      case h :: tail => accu match {
        case Nil => rec(tail, List(h))
        case ha :: ta => if (ha == h) rec(tail, h :: accu) else accu :: rec(tail, List(h))
      }
    }

    rec(in, List())
  }
  /** End of: P09 */

  /** Start of: P10 */
  def encode[T](in: List[T]): List[(T, Int)] =
    (for (e <- pack(in)) yield (e.head, e.length)).toList
  /** End of: P10 */

  /** Start of: P11 */
  def encodeModified[T](in: List[T]): List[Any] =
    (for (e <- pack(in)) yield if (e.length > 1) (e.head, e.length) else e.head)
  /** End of: P11 */

  /** Start of: P12 */
  def decode[T](in: List[(T, Int)]): List[T] =
    (for {
      (ch, count) <- in;
      i <- 1 to count
    } yield ch).toList
  /** End of: P12 */

  /** Start of: P13 */
  def encodeDirect[T](in: List[T]): List[(T, Int)] = in match {
    case Nil => List()
    case h :: t => (in span (_ == in.head)) match {
      case (p1, p2) => (p1.head, p1.length) :: encodeDirect(p2)
    }
  }
  /** End of: P13 */
  
  /** Start of: P14 */
  def duplicate[T](in: List[T]): List[T] = in match {
    case Nil => List()
    case h :: t => List(h, h) ::: duplicate(t)  
  }
  /** End of: P14 */
  
  /** Start of: P15 */
  def duplicateN[T](n: Int, in: List[T]): List[T] = in match {
    case Nil => List()
    case h :: t => List.fill(n)(h) ::: duplicateN(n, t)
  }
  /** End of: P15 */
  
  /** Start of: P16 */
  def drop[T](n: Int, in: List[T]): List[T] = 
    if (n <= 0) throw new IndexOutOfBoundsException
    else in.zip(1 to in.length).filter(x => x._2 % n != 0).unzip._1
  /** End of: P16 */
  
  /** Start of: P17 */
  def split[T](n: Int, in: List[T]): (List[T], List[T]) =
    if (n < 0 || n > in.length) throw new IndexOutOfBoundsException
    else (in.zip(0 until in.length).filter(x => x._2 < n).unzip._1, in.zip(0 until in.length).filter(x => x._2 >= n).unzip._1)
  /** End of: P17 */
    
  /** Start of: P18 */
  def slice[T](from: Int, to: Int, list: List[T]): List[T] =
    if (from < 0 || from > to) throw new IndexOutOfBoundsException
    else list.zip(0 until list.length).filter(_._2 >= from).filter(_._2 <= to).unzip._1 
  /** End of: P18 */
}