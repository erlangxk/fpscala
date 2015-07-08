package coder.simon.scalaz.exercise

object M1 {
  def loop(l: Seq[Int], temp: Seq[Int], result: Seq[Seq[Int]]): Seq[Seq[Int]] = l match {
    case h +: t => temp match {
      case Nil      => loop(t, Seq(h), result)
      case h2 +: t2 => if (h >= h2) loop(t, h +: temp, result) else loop(t, Seq(h), temp +: result)
    }
    case Nil => temp +: result
  }

  def result(ls: Seq[Seq[Int]]) = ls.sortWith((l1, l2) => l1.size > l2.size).headOption.map(_.reverse)

  def main(args: Array[String]) = {
    val l = Seq(1, 2, 3, 5, 6, 7, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11, 12, 3, 4)
    val x = loop(l, Seq.empty[Int], Seq.empty[Seq[Int]])
    println(x)
    val y = result(x)
    println(y)
  }
}