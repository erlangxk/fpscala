package coder.simon.ch2

import org.scalacheck._

object E22 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) {
      return true
    } else {
      for (i <- 1 until as.length) {
        if (!ordered(as(i - 1), as(i))) return false
      }
      return true
    }
  }

  def main(args: Array[String]): Unit = {
    def sortedlist = for {
      l <- Arbitrary.arbitrary[List[Int]]
    } yield l.sorted

    def unsortedlist = for {
      x <- sortedlist
    } yield 7 :: 5 :: x

    val ordered = (i: Int, j: Int) => i <= j

    val ppp2 = Prop.forAll(unsortedlist) {
      x => !isSorted(x.toArray, ordered)
    }

    val ppp = Prop.forAll(sortedlist) {
      l => isSorted(l.toArray, ordered)
    }

    (ppp && ppp2).check

  }

}