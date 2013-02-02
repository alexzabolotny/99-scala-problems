/**
 * Solving problems from this list http://aperiodic.net/phil/scala/s-99/
 */

package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import ninetynineproblems.Lists

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {
  import ninetynineproblems.Lists._

  test("P01, find last element of the list") {
    assert(last(List(1, 2, 3, 4, 5)) === 5)
  }

  test("P01, find last element of the list recursive") {
    assert(lastRec(List(1, 2, 3, 4, 5)) === 5)
  }

  test("P02, find second to last element of the list") {
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
    try {
      penultimate(List())
      penultimate(List(1))
    } catch {
      case _: java.lang.Error =>
    }
  }
  
  test("P03: find the Nth element of the list") {
    assert(nth(3, List(1, 2, 3, 4)) === 4)
    assert(nth(2, List(1, 2, 3, 4)) === 3)
    try {
      nth(-1, List(1, 2, 3))
    } catch {
      case _: IndexOutOfBoundsException => 
    }
    try {
      nth(100, List(1, 2, 3))
    } catch {
      case _: IndexOutOfBoundsException =>
    }
  }
  
  test("P04: find the number of elements in list") {
    assert(length(List(1, 2, 3)) === 3)
    assert(length(List()) === 0)
  }
  
  test("P05: reverse a list") {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
    assert(reverse(List()) === List())
  }
  
  test("P06: test list for palindromness") {
    assert(isPalindrom(List(1, 2, 1)) === true)
    assert(isPalindrom(List(1)) == true)
    assert(isPalindrom(List()) == true)
    assert(isPalindrom(List(1, 2)) === false)
    assert(isPalindrom((1 to 1000).toList ::: (1 to 999).reverse.toList) === true)
  }
}