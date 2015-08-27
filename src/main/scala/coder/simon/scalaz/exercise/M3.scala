package coder.simon.scalaz.exercise

object M3 {

  type Cords = Vector[(Int, Int)]
  val acc = Vector.empty[(Int, Int)]

  def gen2(maxX: Int, maxY: Int)(n: (Int, Int)) = {
    def loop(x: Int, y: Int, result: Cords): Cords = {
      if (x <= maxX && y <= maxY) loop(x + 1, y + 1, result :+ (x, y)) else result
    }
    loop(n._1, n._2, acc)
  }

  def gen1(maxX: Int, maxY: Int) = {
    val l1 = for (i <- maxX to 0 by -1) yield (i, 0)
    val l2 = for (i <- 1 to maxY) yield (0, i)
    l1 ++ l2
  }

  def main(args: Array[String]) {
    val upperX = 3;
    val upperY = 3;
    val path = gen1(upperX, upperY) map gen2(upperX, upperY)
    for (p <- path) {
      println(p)
    }
  }
}