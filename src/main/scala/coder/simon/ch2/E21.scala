package coder.simon.ch2

import scala.annotation.tailrec

object E21 {

  def fib(n: Int): Int = {
    @tailrec
    def go(a: Int, b: Int, i: Int): Int = i match {
      case 0 => a
      case 1 => b
      case _ => go(b, a + b, i - 1)
    }
    go(0, 1, n)
  }

  def fib2(n: Int): Int = {
    if (n < 2) n
    else fib2(n - 1) + fib2(n - 2)
  }

  def main(args: Array[String]): Unit = {
    val fibs = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
    for (i <- 0 to (fibs.size - 1)) {
      assert(fib(i) == fibs(i))
      assert(fib2(i) == fibs(i))
    }
  }

}