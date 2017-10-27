package coder.simon.slots.game1

import coder.simon.slots.common.Collapse
import coder.simon.slots.common.Matrix
import coder.simon.slots.common.OneLineResult
import coder.simon.slots.common.Result
import coder.simon.slots.common.Spin
import coder.simon.slots.common.Symbol
import coder.simon.slots.common.Utils;

object FeatureGame extends Common {

  private val random = new java.util.Random
  private val reels = Utils.toSymbolArray(Array(
    Array(5, 2, 1, 5, 9, 4, 1, 8, 3, 6, 1, 7, 2, 1, 6, 3, 7, 4, 1, 6, 3, 9, 2, 6, 3, 7, 1, 2, 4, 3, 2, 7, 4),
    Array(3, 7, 6, 9, 5, 2, 1, 6, 4, 1, 5, 10, 3, 5, 4, 1, 6, 2, 4, 7, 8, 9, 2, 5, 1, 2, 7, 1, 5, 4, 3, 1, 8, 2, 4),
    Array(6, 1, 5, 4, 3, 9, 1, 7, 4, 6, 1, 10, 3, 2, 6, 8, 1, 5, 2, 7, 1, 9, 5, 4, 2, 5, 1, 4, 8, 2, 7, 3, 2, 4),
    Array(2, 8, 3, 5, 1, 7, 2, 6, 3, 8, 5, 4, 2, 7, 3, 2, 10, 6, 2, 4, 5, 3, 4, 6, 8, 2, 1, 5, 3, 1, 6, 4, 7, 1),
    Array(8, 3, 2, 6, 4, 3, 2, 5, 7, 4, 2, 5, 3, 7, 1, 5, 2, 6, 3, 4, 7, 1, 6, 3, 5, 4, 7, 6, 1, 5, 4)))

  override val reelsInfo = Spin.reelsInfo(lenOfReel, reels);

  val payTable = Utils.toSymbolPayTable(Map(
    1 -> Map(5 -> 65, 4 -> 20, 3 -> 5),
    2 -> Map(5 -> 80, 4 -> 25, 3 -> 10),
    3 -> Map(5 -> 80, 4 -> 25, 3 -> 10),
    4 -> Map(5 -> 100, 4 -> 35, 3 -> 15),
    8 -> Map(5 -> 800, 4 -> 300, 3 -> 50)))

  val scatterPayTable = Utils.toSymbolPayTable(Map(
    5 -> Map(5 -> 5, 4 -> 3, 3 -> 1),
    6 -> Map(5 -> 8, 4 -> 5, 3 -> 2),
    7 -> Map(5 -> 15, 4 -> 8, 3 -> 3)))

  private val S9 = Symbol(9);
  private val scatters = Set(Symbol(5), Symbol(6), Symbol(7), S9);
  private val wild = Symbol(10)

  private val subst = (w: Symbol, s: Symbol) => if (w == wild && !scatters(s)) Some(s) else None

  override def oneLineResult(name: String, line: List[Symbol]) = {
    val (s, c) = Utils.parseLine(Utils.substWild(subst))(line);
    Utils.getMul(payTable)(s, c).fold(List.empty[OneLineResult])(mul => List(OneLineResult(name, s, c, mul)))
  }

  def randomFreespin() = {
    val v = random.nextDouble();
    if (v <= 0.350) {
      4
    } else if (v <= 0.665) {
      3
    } else {
      2
    }
  }

  def calcMatrix(linebet: BigDecimal, totalbet: BigDecimal, matrix: Matrix): Collapse.SpinResult = {
    val (reelArray, linesMul, linesCells) = calcLinesResult(reels, matrix)

    val countScatterResult = Result.countScatter(scatters, reelArray);
    val scatterResult = Result.scatterResult(countScatterResult, Utils.getMul(scatterPayTable))(reelArray);
    val totalMul = Result.totalScatterMul(scatterResult);
    val vanishedScatters = Result.vanishedScatters(scatterResult);
    val feature = if (countScatterResult.get(S9).exists(_ >= 3)) randomFreespin() else 0
    val scatterCollapse = if (feature > 0) S9 :: vanishedScatters else vanishedScatters
    val scatterCells = Collapse.scatterCells(Set(scatterCollapse: _*), reelArray)

    val newMatrix = Collapse.newMatrix(matrix, Set((linesCells ++ scatterCells): _*), reelsInfo);
    Collapse.SpinResult(linebet * linesMul, totalbet * totalMul, feature, newMatrix);
  }

  def spin(linebet: BigDecimal) = {
    val totalbet = linebet * totalLines
    val result = calcMatrix(linebet, totalbet, Spin.idxMatrix(reelsInfo))
    collapse(linebet, totalbet, reelsInfo, result)
  }
}

object Main3 extends App {
  println(FeatureGame.randomFreespin());
}