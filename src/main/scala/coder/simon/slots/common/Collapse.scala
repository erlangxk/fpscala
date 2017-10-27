package coder.simon.slots.common

object Collapse extends App {

  case class SpinResult(lineWin: BigDecimal, scatterWin: BigDecimal, feature: Int, matrix: Option[Matrix])

  def scatterCells(scatters: Set[Symbol], reelArray: ReelArray): List[Cell] = {
    val result = List.empty[Cell];
    if (scatters.isEmpty) result else reelArray.zipWithIndex.foldLeft(result) {
      case (coords, (reel, x)) =>
        reel.zipWithIndex.foldLeft(coords) {
          case (acc, (s, y)) => if (scatters(s)) Cell(x, y) :: acc else acc
        }
    }
  }

  def linesCells(linesResult: LineResult, lines: Lines): List[Cell] = linesResult.flatMap(r => lines(r.name).take(r.count))

  private def pad(size: Int, start: Int, len: Int) = {
    val first = start - size
    (0 until size).map(i => (first + i + len) % len).toList
  }

  private def newMatrixImpl(oldMatrix: Matrix, vanishCells: Set[Cell], reels: Seq[ReelInfo]): Matrix =
    oldMatrix.zipWithIndex.foldRight(List.empty[List[Int]]) {
      case ((reel, x), matrix) =>
        val left = reel.zipWithIndex.foldRight(List.empty[Int]) {
          case ((idx, y), acc) => if (vanishCells(Cell(x, y))) acc else idx :: acc
        }
        (pad(reel.length - left.length, reel.head, reels(x).numOfSymbols) ++ left) :: matrix
    }

  def newMatrix(oldMatrix: Matrix, vanishCells: Set[Cell], reelsInfo: Seq[ReelInfo]): Option[Matrix] =
    if (vanishCells.isEmpty) None else Some(newMatrixImpl(oldMatrix, vanishCells, reelsInfo))

}