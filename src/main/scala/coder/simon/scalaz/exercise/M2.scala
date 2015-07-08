package coder.simon.scalaz.exercise

object M2 {

  def loop(l: Seq[Int], temp: Seq[Seq[Int]]): Seq[Seq[Int]] = l match {
    case h +: t => temp match {
      case Nil => loop(t, Seq(Seq(h)))
      case _   => loop(t, step(h, temp))
    }
    case Nil => temp
  }

  def step(v: Int, r: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    var added = false
    val n = r map (s => if (v >= s.head) { added = true; v +: s } else s)
    if (added) n else Seq(v) +: r
  }

  def max(l: Seq[Seq[Int]]) = l.sortWith { (l1, l2) => l1.size > l2.size }.head

  def main(args: Array[String]) = {
    val l = Seq(1, 2, 3, 5, 6, 7, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11, 12, 3, 4)
    val x = max(loop(l, Seq.empty[Seq[Int]]))
    println(x.size, x)
  }
}