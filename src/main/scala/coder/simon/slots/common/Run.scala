package coder.simon.slots.common

object Run {

  type SpinFunc = () => (BigDecimal, BigDecimal)

  def start(times: Int, fun: SpinFunc)={
    def loop(cost: BigDecimal, win: BigDecimal, n: Int): (BigDecimal, BigDecimal) = {
      if (n > 0) {
        val (c, w) = fun();
        loop(cost + c, win + w, n - 1)
      } else (cost, win)
    }
    val t0 = System.currentTimeMillis()
    val result= loop(0, 0, times)
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

}