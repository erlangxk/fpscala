package coder.simon.slots.common

object Utils {

  type Substitute = (Symbol, Symbol) => Option[Symbol]
  
  def toSymbolArray(reelsConfig: Array[Array[Int]]) = reelsConfig.map(l => l.map(Symbol))
  
  def toSymbolPayTable(rawPayTable: Map[Int, Map[Int, Int]]) = rawPayTable.map { case (s, m) => (Symbol(s), m) }.toMap
  def toLinesDef1(rawLines: Map[String, List[Int]]) = rawLines.mapValues(line => line.zipWithIndex.map { case (y, x) => Cell(x, y) })
  def toLinesDef2(rawLines: Map[String, List[Int]]) = rawLines.mapValues(line => line.map { x => Cell(x, 0) })

  def substSimple(first: Symbol, second: Symbol): Option[Symbol] = if (first == second) Some(second) else None
  def substWild(subst: Substitute)(first: Symbol, second: Symbol) = substSimple(first, second).orElse(subst(first, second)).orElse(subst(second, first))

  def parseLine(subst: Substitute)(symbols: List[Symbol]) = {
    def loop(ss: List[Symbol], rep: Symbol, count: Int): (Symbol, Int) = ss match {
      case head :: tail => subst(rep, head).fold((rep, count))(r => loop(tail, r, count + 1))
      case Nil          => (rep, count)
    }
    loop(symbols.tail, symbols.head, 1)
  }

  def parseScatter(isScatter: Symbol => Boolean)(reelArray: ReelArray) =
    reelArray.foldLeft(Map.empty[Symbol, Int]) {
      (map, reel) => reel.foldLeft(map)((acc, s) => if (isScatter(s)) acc.updated(s, acc.getOrElse(s, 0) + 1) else acc)
    }

  def parseFloat(symbol: Symbol)(symbols: List[Symbol]) = (symbol, symbols.count(_ == symbol))

  def getMul(payTable: Map[Symbol, Map[Int, Int]])(symbol: Symbol, count: Int) = payTable.get(symbol).flatMap(m2 => m2.get(count))
}