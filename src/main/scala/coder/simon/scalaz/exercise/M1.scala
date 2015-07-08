package coder.simon.scalaz.exercise

object M1 {

  def max(a: Int, b: Int) = if (a > b) a else b

  def loop(l: Seq[Int], temp: Seq[Int], result: Int): Int = l match {
    case h +: t => temp match {
      case Nil      => loop(t, Seq(h), result)
      case h2 +: t2 => if (h >= h2) loop(t, h +: temp, result) else loop(t, Seq(h), max(temp.size, result))
    }
    case Nil => max(temp.size, result)
  }

  def main(args: Array[String]) = {
    val l = Seq(1, 2, 3, 5, 6, 7, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11, 12, 3, 4)
    val x = loop(l, Seq.empty[Int], 0)
    println(x)
  }
}