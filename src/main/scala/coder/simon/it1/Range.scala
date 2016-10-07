package coder.simon.it1

object TaxRate {
  case class Range(val min: Double = Double.MinValue, val max: Double = Double.MaxValue) {
    def include(number: Double): Boolean = number > min && number <= max
  }

  private val range1 = Range(max = 2.0)
  private val range2 = Range(min = 2.0, max = 5.0)
  private val range3 = Range(min = 5.0)

  val rateTable = Map(
    "Europe" -> Map(range1 -> 1.00, range2 -> 1.20, range3 -> 2.00),
    "USA" -> Map(range1 -> 0.75, range2 -> 0.90, range3 -> 1.50),
    "Japan" -> Map(range1 -> 0.70, range2 -> 0.80, range3 -> 1.35))

  def calcRate(from: String, capacity: Double) = for (m <- rateTable.get(from); p <- m.find(_._1.include(capacity))) yield p._2
}

object Calculator {
  def totalPrice(price: BigDecimal)(rate: Double): BigDecimal = {
    val tax = importTax(price, rate)
    price + tax + vat(tax + price)
  }

  def importTax(price: BigDecimal, rate: Double) = price * rate
  def vat(price: BigDecimal) = price * 0.12
  def usd2Pesos(usd: BigDecimal) = usd * 47.0

  def totalPriceInPesos(from: String, capacity: Double, price: BigDecimal) = TaxRate.calcRate(from, capacity).map(totalPrice(price)).map(usd2Pesos)
}

object Test extends App {
  val calc = (Calculator.totalPriceInPesos _).tupled

  val benzG65 = ("Europe", 6.0, BigDecimal(217900))
  val hondaJazz = ("Japan", 1.5, BigDecimal(19490))
  val jeepWrangler = ("USA", 3.6, BigDecimal(36995))
  val cheryQQ = ("China", 1.0, BigDecimal(6000))

  val p1 = calc(benzG65)
  assert(p1 == Some(34410768.00))

  val p2 = calc(hondaJazz)
  assert(p2 == Some(1744121.12))

  val p3 = calc(jeepWrangler)
  assert(p3 == Some(3700091.92))

  val p4 = calc(cheryQQ)
  assert(p4 == None)

  assert(false, "all tests passed excpet this one")
}