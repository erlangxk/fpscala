package coder.simon.scalaz.exercise

object M1 {

  val l = List(1, 2, 3, 5, 6, 7, 4, 5, 6, 7, 8, 9, 10, 11, 12, 3, 4)

  def loop(l: List[Int], temp: List[Int], result: List[List[Int]]): List[List[Int]] = l match {
    case h :: t => temp match {
      case Nil      => loop(t, List(h), result)
      case h2 :: t2 => if (h > h2) loop(t, h :: temp, result) else loop(t, List(h), temp :: result)
    }
    case Nil => temp :: result
  }

  def result(ls: List[List[Int]]) = {
    ls.sortWith((l1, l2) => l1.size > l2.size).headOption.map(_.reverse)
  }

  def main(args: Array[String]) = {
    val x = loop(l, List.empty[Int], List.empty[List[Int]])
    println(x)
    val y = result(x)
    println(y)

  }

}