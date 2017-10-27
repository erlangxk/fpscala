package coder.simon.slots.common

object Result {

  type LineMulFunc = (String, List[Symbol]) => List[OneLineResult]

  def linesResult(lines: Lines, oneLineResult: LineMulFunc)(reelArray: ReelArray): LineResult = {
    lines.foldLeft(List.empty[OneLineResult]) {
      case (acc, (name, coords)) => {
        val ss = coords.map(c => reelArray(c.x)(c.y))
        oneLineResult(name, ss) ::: acc
      }
    }
  }

  def totalLineMul(results: List[OneLineResult]) = results.foldLeft(0)((acc, r) => r.mul + acc)

  type ScatterMulFunc = (Symbol, Int) => Option[Int]

  def countScatter(isScatter: Symbol => Boolean, reelArray: ReelArray) = {
    reelArray.flatMap(reel => reel.filter(isScatter)).groupBy(identity).mapValues(_.length);
  }

  def scatterResult(countScatterResult: Map[Symbol, Int], oneScatterResult: ScatterMulFunc)(reelArray: ReelArray): ScatterResult = {
    countScatterResult.foldLeft(List.empty[OneScatterResult]) {
      case (acc, (s, c)) =>
        oneScatterResult(s, c).fold(acc)(mul => OneScatterResult(s, c, mul) :: acc)
    }
  }

  def totalScatterMul(results: List[OneScatterResult]) = results.foldLeft(0)((acc, r) => r.mul + acc)

  def vanishedScatters(results: List[OneScatterResult]) = results.map(r => r.symbol)
}