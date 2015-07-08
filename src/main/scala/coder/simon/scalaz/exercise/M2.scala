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
    def loop(l: Seq[Int]) = if (v >= l.head) Set(v +: l) else Set(v +: l.dropWhile(_ > v), l)
    (r.toSet flatMap loop).toSeq
  }

  def max(l: Seq[Seq[Int]]) = l.sortWith { (l1, l2) => l1.size > l2.size }.head

  def main(args: Array[String]) = {
    val l = Seq(1, 2, 3, 5, 6, 7, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11, 12, 3, 4)
    test(l)
    println("xxxxxxxx")
    val l2 = Seq(100, 101, 102, 1, 2, 10, 4, 5)
    test(l2)
  }

  def test(t: Seq[Int]) = {
    println(t)
    val x = loop(t, Seq.empty[Seq[Int]])
    println(x)
    val y = max(x)
    println(y.size, y)
  }
}