package coder.simon.ch2

import org.scalacheck.Prop.forAll

object E22 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length == 0 || as.length == 1) true
    else {
      for (i <- 2 until as.length) {
        if (!ordered(as(i - 1), as(i))) return false
      }
      return true
    }
  }

  def main(args: Array[String]): Unit = {
    assert(isSorted(Array(1, 2, 3, 4, 5), (i: Int, j: Int) => i < j))
    assert(isSorted(Array(), (i: Int, j: Int) => i < j))
    assert(isSorted(Array(), (i: Int, j: Int) => i > j))
    assert(isSorted(Array(2), (i: Int, j: Int) => i < j))
    assert(isSorted(Array(123), (i: Int, j: Int) => i > j))
    assert(isSorted(Array(1, 2, 3, 7), (i: Int, j: Int) => i < j))
    assert(!isSorted(Array(1, 2, 3, 7, 3), (i: Int, j: Int) => i < j))
    assert(!isSorted(Array(1, 2, 3, 4, 5, 6), (i: Int, j: Int) => i > j))

    val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
      l1.size + l2.size == (l1 ::: l2).size
    }
    
    propConcatLists.check
  }

}