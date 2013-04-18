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
  
  test("P07: flatten list structure") {
    //assert(flatten(List(List(1, 2), 3, List(3))) === List(1, 2, 3, 3))
    assert(flatten(List(1, 2, List(1, 3))) === List(1, 2, 1, 3))
  }

  test("P08: compress the list") {
    assert(compress(List('a', 'b', 'c', 'c', 'c', 'd', 'd')) === List('a', 'b', 'c', 'd'))
    assert(compress(List()) === List())
    assert(compress(List('b', 'a', 'a', 'b', 'b', 'b')) === List('b', 'a', 'b'))
  }

  test("P09: pack object of the list") {
    assert(pack(List('a', 'a', 'a', 'b', 'c', 'c', 'c')) === List(List('a', 'a', 'a'), List('b'), List('c', 'c', 'c')))
    assert(pack(List('a')) === List(List('a')))
  }

  test("P10: consecutive character frequency") {
    assert(encode(List('a', 'a', 'a', 'b', 'c', 'c', 'c', 'a')) === List(('a', 3), ('b', 1), ('c', 3), ('a', 1)))
    assert(encode(List.fill(100000)('a')) === List(('a', 100000)))
  }

  test("P11: modified encoding") {
    assert(encodeModified(List('a', 'a', 'a', 'b', 'c', 'c')) === List(('a', 3), 'b', ('c', 2)))
  }

  test("P12: decode run-length encoded list") {
    assert(decode(List(('a', 2), ('b', 1), ('c', 3))) === List('a', 'a', 'b', 'c', 'c', 'c'))
  }

  test("P13: encode directly") {
    assert(encodeDirect(List('a', 'a', 'b', 'c', 'c', 'c')) === List(('a', 2), ('b', 1), ('c', 3)))
    assert(encodeDirect(List()) === List())
    assert(encodeDirect(List('a')) === List(('a', 1)))
  }

  test("P14: duplicate elements of list") {
    assert(duplicate(List('a', 'b', 'c')) === List('a', 'a', 'b', 'b', 'c', 'c'))
    assert(duplicate(List()) === List())
  }

  test("P15: duplicate elements of list given number of times") {
    assert(duplicateN(3, List('a', 'b')) === List('a', 'a', 'a', 'b', 'b', 'b'))
  }
  
  test("P16: drop every Nth element from list") {
    assert(drop(2, List('a', 'b', 'c', 'd', 'e')) === List('a', 'c', 'e'))
  }
  
  test("P17: split list") {
    assert(split(3, List('a', 'b', 'c', 'd')) === (List('a', 'b', 'c'), List('d')))
  }
  
  test("P18: slice list") {
    assert(slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')) === List('d', 'e', 'f', 'g', 'h'))
  }
}