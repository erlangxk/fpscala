package coder.simon.slots.common

import java.util.Random

object Spin {
  type RNG = Int => Int

  def randomRNG(random: Random)(max: Int) = random.nextInt(max);

  def idxMatrix(reelInfos: Seq[ReelInfo]): Matrix = randomMatrix(randomRNG(new java.util.Random()))(reelInfos)

  def randomMatrix(random: RNG)(reelInfos: Seq[ReelInfo]): Matrix = {
    def gen(max: Int, times: Int) = {
      val start = random(max);
      (0 until times).map(i => (start + i) % max).toIndexedSeq
    }
    reelInfos.map(ri => gen(ri.numOfSymbols, ri.lenOfReel)).toIndexedSeq
  }

  def crop(reels: ReelArray, matrix: Matrix): ReelArray = {
    def oneReel(column: Seq[Int], reel: Array[Symbol]) = column.map(i => reel(i)).toArray
    reels.zipWithIndex.map { case (reel, x) => oneReel(matrix(x), reel) }
  }

  def reelsInfo(lenOfReel: Int, reels: ReelArray) = reels.map(r => ReelInfo(r.length, lenOfReel)).toSeq

  def randomSpin(lenOfReel: Int, reels: ReelArray) = {
    val matrix = idxMatrix(reelsInfo(lenOfReel, reels))
    crop(reels, matrix)
  }
}