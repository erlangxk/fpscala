package coder.simon.slots.game2

import _root_.coder.simon.slots.common.{ Symbol, Utils, OneLineResult, Spin, Result, Run };

object Game {

  private val lenOfReel = 1
  private val totalLines = 8

  private val floatingSymbol = Symbol(8)

  private val reels = Utils.toSymbolArray(Array(
    Array(0, 8, 1, 5, 2, 3, 4, 6, 1, 8, 1, 0, 4, 3, 0, 5, 1, 4, 3, 2, 7, 0, 1, 4, 6, 3, 5, 1, 2, 4, 0, 1, 0),
    Array(0, 8, 1, 5, 2, 3, 4, 6, 1, 8, 1, 0, 4, 3, 0, 5, 1, 4, 3, 2, 7, 0, 1, 4, 6, 3, 5, 1, 2, 4, 0, 1, 0),
    Array(0, 8, 1, 5, 2, 3, 4, 6, 1, 8, 1, 0, 4, 3, 0, 5, 1, 4, 3, 2, 7, 0, 1, 4, 6, 3, 5, 1, 2, 4, 0, 1, 0),

    Array(0, 1, 4, 0, 1, 1, 0, 3, 4, 1, 0, 8, 2, 0, 1, 2, 5, 2, 1, 3, 1, 3, 2, 2, 1, 3, 5, 1, 2, 3, 0, 2, 0, 1, 7, 0, 2, 6, 2, 0),
    Array(0, 1, 4, 0, 1, 1, 0, 3, 4, 1, 0, 8, 2, 0, 1, 2, 5, 2, 1, 3, 1, 3, 2, 2, 1, 3, 5, 1, 2, 3, 0, 2, 0, 1, 7, 0, 2, 6, 2, 0),
    Array(0, 1, 4, 0, 1, 1, 0, 3, 4, 1, 0, 8, 2, 0, 1, 2, 5, 2, 1, 3, 1, 3, 2, 2, 1, 3, 5, 1, 2, 3, 0, 2, 0, 1, 7, 0, 2, 6, 2, 0),

    Array(0, 7, 3, 8, 1, 5, 3, 2, 3, 4, 3, 6, 0, 1, 0, 6, 2, 0, 2, 5, 3, 0, 4, 3, 1, 0, 4, 3, 6, 0, 1, 4, 2),
    Array(0, 7, 3, 8, 1, 5, 3, 2, 3, 4, 3, 6, 0, 1, 0, 6, 2, 0, 2, 5, 3, 0, 4, 3, 1, 0, 4, 3, 6, 0, 1, 4, 2),
    Array(0, 7, 3, 8, 1, 5, 3, 2, 3, 4, 3, 6, 0, 1, 0, 6, 2, 0, 2, 5, 3, 0, 4, 3, 1, 0, 4, 3, 6, 0, 1, 4, 2)))

  private val lines = Utils.toLinesDef2(Map(
    "line1" -> List(3, 4, 5),
    "line2" -> List(0, 1, 2),
    "line3" -> List(6, 7, 8),
    "line4" -> List(0, 3, 6),
    "line5" -> List(1, 4, 7),
    "line6" -> List(2, 5, 8),
    "line7" -> List(0, 4, 8),
    "line8" -> List(2, 4, 6)))

  private val payTable = Utils.toSymbolPayTable(Map(
    0 -> Map(3 -> 10),
    1 -> Map(3 -> 20),
    2 -> Map(3 -> 30),
    3 -> Map(3 -> 40),
    4 -> Map(3 -> 80),
    5 -> Map(3 -> 100),
    6 -> Map(3 -> 200),
    7 -> Map(3 -> 1000)))

  private val floatingSymbol8PayTable = Map(floatingSymbol -> Map(3 -> 200, 2 -> 10, 1 -> 2))

  private def oneLineResult(name: String, line: List[Symbol]) = {
    var result = List.empty[OneLineResult];
    val (s1, c1) = Utils.parseLine(Utils.substSimple)(line)
    Utils.getMul(payTable)(s1, c1).foreach { mul =>
      result = OneLineResult(name, s1, c1, mul) :: result
    }
    val (s2, c2) = Utils.parseFloat(floatingSymbol)(line)
    Utils.getMul(floatingSymbol8PayTable)(s2, c2).foreach { mul =>
      result = OneLineResult(name, s2, c2, mul) :: result
    }
    result
  }

  def spin(linebet: BigDecimal) = {
    val reelArray = Spin.randomSpin(lenOfReel, reels)
    val linesResult = Result.linesResult(lines, oneLineResult)(reelArray)
    val mul = Result.totalLineMul(linesResult);
    (linebet * totalLines, linebet * mul)
  }
}

object Main extends App {
  val result = Run.start(1000000, () => Game.spin(1))
  println(result)
  println(result._2 / result._1);
}