package coder.simon.scalaz.exercise

object M3 {

  val upperX = 3;
  val upperY = 3;

  def gen(n: (Int, Int)) = {
    def loop(dot: (Int, Int), result: Vector[(Int, Int)]): Vector[(Int, Int)] = {
      val (x, y) = dot
      if (x <= upperX && y <= upperY) loop((x + 1, y + 1), result :+ dot)
      else result
    }
    loop(n, Vector.empty[(Int, Int)])
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