package coder.simon.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, next) = rng.nextInt
    val absv = if (v == Int.MinValue) Int.MaxValue else math.abs(v)
    (absv, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, next) = nonNegativeInt(rng)
    val x = if (v == Int.MaxValue) v - 1 else v
    (x.toDouble / Int.MaxValue, next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (v1, n1) = rng.nextInt
    val (d1, n2) = double(n1)
    ((v1, d1), n2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (v1, n1) = rng.nextInt
    val (d1, n2) = double(n1)
    ((d1, v1), n2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, gg: RNG, r: List[Int]): (List[Int], RNG) = {
      if (n > 0) {
        val (v, next) = gg.nextInt
        loop(n - 1, next, v :: r)
      } else (r, gg)
    }
    loop(count, rng, List.empty[Int])
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1234)
    println(rng.nextInt)
    println(rng.nextInt)

    val (x, next) = rng.nextInt
    println((x, next))
    println(next.nextInt)
  }
}