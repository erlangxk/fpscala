package coder.simon.slots.game1

import coder.simon.slots.common.Collapse
import coder.simon.slots.common.Matrix
import coder.simon.slots.common.OneLineResult
import coder.simon.slots.common.ReelInfo
import coder.simon.slots.common.Symbol
import coder.simon.slots.common.Utils
import coder.simon.slots.common.Spin
import coder.simon.slots.common.Result
import coder.simon.slots.common.ReelArray

trait Common {
  protected val lenOfReel = 3
  protected val totalLines = 30

  protected val lines = Utils.toLinesDef1(Map(
    "line1" -> List(1, 1, 1, 1, 1),
    "line2" -> List(0, 0, 0, 0, 0),
    "line3" -> List(2, 2, 2, 2, 2),
    "line4" -> List(0, 0, 1, 1, 1),
    "line5" -> List(2, 2, 1, 1, 1),
    "line6" -> List(0, 0, 2, 2, 2),
    "line7" -> List(2, 2, 0, 0, 0),
    "line8" -> List(1, 1, 0, 1, 2),
    "line9" -> List(1, 1, 2, 1, 0),
    "line10" -> List(0, 1, 0, 0, 0),
    "line11" -> List(2, 1, 2, 2, 2),
    "line12" -> List(1, 0, 0, 0, 1),
    "line13" -> List(1, 2, 2, 2, 1),
    "line14" -> List(1, 0, 1, 0, 0),
    "line15" -> List(1, 2, 1, 2, 2),
    "line16" -> List(0, 1, 1, 2, 2),
    "line17" -> List(2, 1, 1, 0, 0),
    "line18" -> List(1, 0, 2, 1, 1),
    "line19" -> List(1, 2, 0, 1, 1),
    "line20" -> List(0, 1, 2, 2, 2),
    "line21" -> List(2, 1, 0, 0, 0),
    "line22" -> List(0, 2, 0, 1, 2),
    "line23" -> List(2, 0, 2, 1, 0),
    "line24" -> List(0, 2, 1, 0, 1),
    "line25" -> List(2, 0, 1, 2, 1),
    "line26" -> List(0, 2, 2, 0, 0),
    "line27" -> List(2, 0, 0, 2, 2),
    "line28" -> List(1, 1, 1, 0, 0),
    "line29" -> List(1, 1, 1, 2, 2),
    "line30" -> List(0, 2, 2, 2, 2)))

  def reelsInfo: Seq[ReelInfo]

  def calcMatrix(linebet: BigDecimal, totalbet: BigDecimal, matrix: Matrix): Collapse.SpinResult

  def collapse(linebet: BigDecimal,  totalbet:BigDecimal, reelsInfo: Seq[ReelInfo], result: Collapse.SpinResult): (BigDecimal, BigDecimal, Int) = {
    result.matrix match {
      case Some(m) =>
        val newResult = calcMatrix(linebet, totalbet, m)
        val lineWin = result.lineWin + newResult.lineWin
        val scatterWin = result.scatterWin + newResult.scatterWin
        val feature = result.feature + newResult.feature
        val next = Collapse.SpinResult(lineWin, scatterWin, feature, newResult.matrix)
        collapse(linebet, totalbet, reelsInfo, next)
      case None => (result.lineWin, result.scatterWin, result.feature);
    }
  }

  def oneLineResult(name: String, line: List[Symbol]): List[OneLineResult]

  def calcLinesResult(reels: ReelArray, matrix: Matrix) = {
    val reelArray = Spin.crop(reels, matrix);
    val linesResult = Result.linesResult(lines, oneLineResult)(reelArray)
    val linesMul = Result.totalLineMul(linesResult);
    val linesCells = Collapse.linesCells(linesResult, lines);
    (reelArray, linesMul, linesCells)
  }
}