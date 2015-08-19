package coder.simon.scalaz.exercise

/**
 * @author simon
 */
object M4 {

  def p(n: Int): BigInt = {
    def loop(m: Int, r: BigInt): BigInt = {
      if (m < 2) r else loop(m - 1, m * r)
    }
    loop(n, 1)
  }

  def main(args: Array[String]): Unit = {
    println(p(52))
    println(math.pow(2, 224))
  }

}