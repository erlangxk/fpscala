package coder.simon.slots.game1

import coder.simon.slots.common.Collapse
import coder.simon.slots.common.Matrix
import coder.simon.slots.common.OneLineResult
import coder.simon.slots.common.Result
import coder.simon.slots.common.Spin
import coder.simon.slots.common.Symbol
import coder.simon.slots.common.Utils;

object MainGame extends Common {

  private val reels = Utils.toSymbolArray(Array(
    Array(5, 2, 1, 9, 4, 8, 3, 1, 7, 2, 5, 6, 3, 9, 7, 4, 1, 8, 3, 2, 6, 3, 7, 1, 2, 6, 3, 2),
    Array(3, 7, 6, 1, 2, 9, 4, 1, 5, 10, 3, 4, 6, 1, 2, 4, 9, 7, 8, 2, 5, 6, 2, 7, 1, 5, 3, 4, 6, 3, 1, 8, 2, 1, 5, 4, 3, 1, 8, 2),
    Array(6, 1, 4, 3, 9, 1, 7, 4, 6, 1, 3, 2, 10, 1, 8, 5, 2, 1, 8, 4, 2, 5, 9, 1, 8, 4, 2, 8, 3, 1, 7, 3, 2, 5, 4, 2),
    Array(2, 8, 3, 1, 7, 6, 2, 3, 10, 1, 8, 5, 4, 2, 7, 1, 4, 8, 8, 3, 2, 6, 2, 5, 3, 4, 8, 5, 2, 6, 4, 1, 8, 5, 3, 1, 6, 7, 1),
    Array(8, 3, 2, 6, 3, 2, 5, 7, 4, 2, 5, 3, 7, 1, 6, 8, 3, 2, 7, 5, 4, 3, 6, 1, 4, 7, 1, 6, 3, 7, 1, 5, 8, 6, 4, 1)))

  override val reelsInfo = Spin.reelsInfo(lenOfReel, reels);

  val payTable = Utils.toSymbolPayTable(Map(
    8 -> Map(5 -> 800, 4 -> 300, 3 -> 50),
    7 -> Map(5 -> 200, 4 -> 60, 3 -> 30),
    6 -> Map(5 -> 150, 4 -> 50, 3 -> 25),
    5 -> Map(5 -> 125, 4 -> 40, 3 -> 20),
    4 -> Map(5 -> 100, 4 -> 35, 3 -> 15),
    3 -> Map(5 -> 80, 4 -> 25, 3 -> 10),
    2 -> Map(5 -> 80, 4 -> 25, 3 -> 10),
    1 -> Map(5 -> 65, 4 -> 20, 3 -> 5)))

  private val wild = Symbol(10);
  private val scatter = Symbol(9);

  override def oneLineResult(name: String, line: List[Symbol]) = {
    def subst(first: Symbol, second: Symbol) = if (first == wild && second != scatter) Some(second) else None
    val (s, c) = Utils.parseLine(Utils.substWild(subst))(line);
    Utils.getMul(payTable)(s, c).fold(List.empty[OneLineResult])(mul => List(OneLineResult(name, s, c, mul)))
  }

  override def calcMatrix(linebet: BigDecimal, totalbet: BigDecimal, matrix: Matrix): Collapse.SpinResult = {
    val (reelArray, linesMul, linesCells) = calcLinesResult(reels, matrix)

    val isScatterMoreThan3 = Result.countScatter(_ == scatter, reelArray).get(scatter).exists(_ >= 3)
    val (scatterCollapse, feature) = if (isScatterMoreThan3) (Set(scatter), 6) else (Set.empty[Symbol], 0)
    
    val scatterCells = Collapse.scatterCells(scatterCollapse, reelArray)
    val newMatrix = Collapse.newMatrix(matrix, Set((linesCells ++ scatterCells): _*), reelsInfo);
    Collapse.SpinResult(linebet * linesMul, 0, feature, newMatrix);
  }

  def spin(linebet: BigDecimal) = {
    val totalbet = linebet * totalLines
    val result = calcMatrix(linebet, totalbet, Spin.idxMatrix(reelsInfo))
    val (lineWin, scatterWin, feature) = collapse(linebet, totalbet, reelsInfo, result)
    (totalbet, lineWin, scatterWin, feature)
  }

}