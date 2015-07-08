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
    def add(r: Seq[Int]) = if (v >= r.head) {
      added = true; Seq(v +: r)
    } else {
      val l = r.dropWhile(_ > v)
      if (l.isEmpty) Seq(r) else {
        added = true; Seq(v +: l, r)
      }
    }
    val n = r flatMap add
    if (added) n else Seq(v) +: r
  }

  def max(l: Seq[Seq[Int]]) = l.sortWith { (l1, l2) => l1.size > l2.size }.head

  def main(args: Array[String]) = {
    val l = Seq(1, 2, 3, 5, 6, 7, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11, 12, 3, 4)
    test(l)
    println("xxxxxxxx")
    val l2 = Seq(1, 2, 10, 4, 5)
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