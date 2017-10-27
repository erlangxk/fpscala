package coder.simon.slots.game1
import _root_.coder.simon.slots.common.{ Symbol, ReelArray, Utils, Spin, Result, Collapse, Run, OneLineResult, Matrix, ReelInfo };

object Game {
  def spin(linebet: BigDecimal) = {
    val (cost1, lineWin1, scatterWin1, feature1) = MainGame.spin(linebet);
    val (lineWin2, scatterWin2) = (0,0)
    //val (lineWin2, scatterWin2) = loopFeatureSpin(linebet, feature1, 0, 0)
    (cost1, lineWin1 + scatterWin1 + lineWin2 + scatterWin2)
  }

  def loopFeatureSpin(linebet: BigDecimal, times: Int, accLineWin: BigDecimal, accScatterWin: BigDecimal): (BigDecimal, BigDecimal) = {
    if (times > 0) {
      val (lineWin, scatterWin, feature) = FeatureGame.spin(linebet);
      loopFeatureSpin(linebet, times - 1 + feature, lineWin + accLineWin, scatterWin + accScatterWin)
    } else (accLineWin, accScatterWin)
  }
}

object Main extends App {
  val result = Run.start(1000000, () => Game.spin(1))
  println(result)
  println(result._2 / result._1);
}