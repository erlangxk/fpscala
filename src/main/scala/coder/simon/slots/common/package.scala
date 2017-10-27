package coder.simon.slots
import scala.util.Random


package object common {

  case class Symbol(t: Int) extends AnyVal
  case class Cell(x: Int, y: Int)
  case class ReelInfo(numOfSymbols: Int, lenOfReel: Int)

  type Matrix = Seq[Seq[Int]]
  type Lines = Map[String, List[Cell]]

  case class OneLineResult(name:String, symbol: Symbol, count: Int, mul: Int)
  case class OneScatterResult(symbol: Symbol, count: Int, mul: Int)

  type LineResult = List[OneLineResult]
  type ScatterResult = List[OneScatterResult]
  type ReelArray = Array[Array[Symbol]]
  
}