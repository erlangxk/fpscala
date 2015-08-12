package coder.simon.scalaz.exercise

object M3 {

  val upperX = 3;
  val upperY = 3;

  def gen(n: (Int, Int)) = {
    def loop(x: Int, y: Int, result: List[(Int, Int)]): List[(Int, Int)] = {
      if (x <= upperX && y <= upperY) loop(x + 1, y + 1, (x, y) :: result)
      else result
    }
    loop(n._1, n._2, List.empty[(Int, Int)]).reverse
  }

  def main(args: Array[String]) {
    val l1 = for (i <- upperX to 0 by -1) yield (i, 0)
    val l2 = for (i <- 1 to upperY) yield (0, i)
    val l = l1 ++ l2
    val path = l map gen
    for (p <- path) {
      println(p)
    }
  }

}