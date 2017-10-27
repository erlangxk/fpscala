package coder.simon.slots.game3

import _root_.coder.simon.slots.common.{ Symbol, Utils, OneLineResult, Spin, Result, Run };

object Game {
  private val lenOfReel = 3
  private val totalLines = 9

  private val reels = Utils.toSymbolArray(Array(
    Array(6, 2, 9, 8, 3, 0, 10, 9, 5, 1, 7, 0, 5, 6, 2, 8, 4, 3, 8, 7, 9, 4, 7, 10, 1, 5, 6, 7, 4, 10, 2, 7, 2, 3, 10, 1, 0),
    Array(9, 3, 5, 7, 4, 10, 2, 5, 6, 7, 8, 3, 11, 8, 9, 11, 3, 2, 9, 11, 0, 6, 2, 11, 1, 6, 4, 5, 10, 4, 6, 1, 2, 8, 10, 6, 1, 0, 7, 2, 0, 8, 1),
    Array(4, 2, 8, 3, 10, 1, 5, 8, 10, 7, 5, 4, 9, 11, 7, 5, 2, 8, 11, 4, 2, 6, 1, 0, 7, 2, 11, 9, 5, 3, 9, 0, 6, 1, 3, 10, 1, 0, 10, 9, 2, 6, 5),
    Array(2, 7, 9, 11, 0, 8, 1, 11, 5, 9, 0, 3, 10, 4, 7, 3, 0, 8, 11, 1, 4, 9, 3, 2, 7, 1, 0, 5, 8, 4, 6, 2, 4, 1, 2, 6, 4, 0, 10, 5, 8, 1, 6),
    Array(8, 7, 0, 4, 10, 6, 4, 0, 6, 1, 3, 7, 5, 10, 6, 4, 2, 9, 1, 6, 2, 9, 5, 1, 3, 6, 2, 9, 3, 5, 8, 2, 8, 3, 0)))

  private val lines = Utils.toLinesDef1(Map(
    "line1" -> List(1, 1, 1, 1, 1),
    "line2" -> List(0, 0, 0, 0, 0),
    "line3" -> List(2, 2, 2, 2, 2),
    "line4" -> List(0, 1, 2, 1, 0),
    "line5" -> List(2, 1, 0, 1, 2),
    "line6" -> List(0, 0, 1, 0, 0),
    "line7" -> List(2, 2, 1, 2, 2),
    "line8" -> List(1, 2, 2, 2, 1),
    "line9" -> List(1, 0, 0, 0, 1)))

  private val payTable = Utils.toSymbolPayTable(Map(
    0 -> Map(5 -> 70, 4 -> 25, 3 -> 10),
    1 -> Map(5 -> 70, 4 -> 25, 3 -> 10),
    2 -> Map(5 -> 80, 4 -> 30, 3 -> 10),
    3 -> Map(5 -> 80, 4 -> 30, 3 -> 10),
    4 -> Map(5 -> 100, 4 -> 45, 3 -> 15),
    5 -> Map(5 -> 100, 4 -> 45, 3 -> 15),
    6 -> Map(5 -> 100, 4 -> 45, 3 -> 15),
    7 -> Map(5 -> 200, 4 -> 60, 3 -> 20, 2 -> 2),
    8 -> Map(5 -> 200, 4 -> 60, 3 -> 20, 2 -> 2),
    9 -> Map(5 -> 300, 4 -> 120, 3 -> 35, 2 -> 5)))

  private val S10 = Symbol(10);
  private val wild = Symbol(11);

  private val scatterPayTable = Map(S10 -> Map(5 -> 85, 4 -> 10, 3 -> 3))

  private val subst = (first: Symbol, second: Symbol) => if (first == wild && second != S10) Some(second) else None

  private def oneLineResult(name: String, line: List[Symbol]) = {
    val (s1, c1) = Utils.parseLine(Utils.substWild(subst))(line)
    Utils.getMul(payTable)(s1, c1).fold(List.empty[OneLineResult])(mul => List(OneLineResult(name, s1, c1, mul)))
  }

  def spin(linebet: BigDecimal) = {
    val reelArray = Spin.randomSpin(lenOfReel, reels)
    val linesResult = Result.linesResult(lines, oneLineResult)(reelArray)
    val lineMul = Result.totalLineMul(linesResult);
    val countScatterResult = Result.countScatter(s => s == S10, reelArray);
    val scatterResult = Result.scatterResult(countScatterResult, Utils.getMul(scatterPayTable))(reelArray)
    val totalMul = Result.totalScatterMul(scatterResult)
    (linebet * totalLines, linebet * lineMul + linebet * totalLines * totalMul)
  }
}

object Main extends App {
  val result = Run.start(1000000, () => Game.spin(1))
  println(result)
  println(result._2 / result._1);
}