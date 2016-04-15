package coder.simon.ch6

/**
 * @author simon
 */
object Gen2 {

  trait RNG {
    def nextInt(): (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  val intRand: Rand[Int] = _.nextInt()

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (v, rngx) = s(rng)
    (f(v), rngx)
  }

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (v, rngx) = s(rng)
    val (v2, rngx2) = f(v)(rngx)
    (v2, rngx2)
  }

  def mapX[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  def map2X[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { x => mapX(rb) { y => f(x, y) } }

  val nonNegativeRand = map(intRand) { i =>
    if (i == Int.MinValue) Int.MaxValue else math.abs(i)
  }

  val doubleRand = map(nonNegativeRand) { i =>
    val v = if (i == Int.MaxValue) (i - 1) else i
    v.toDouble / Int.MaxValue
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (v1, r1) = ra(rng)
    val (v2, r2) = rb(r1)
    (f(v1, v2), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]) = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(intRand, doubleRand)
  val randDoubleInt: Rand[(Double, Int)] = both(doubleRand, intRand)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil    => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil: List[A]))((e, acc) => map2(e, acc)(_ :: _))

  def nonNegtiveLessThan(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeRand(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegtiveLessThan(n)(rng)
  }

  def nonNegtiveLessThan2(n: Int): Rand[Int] = flatMap(nonNegativeRand) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegtiveLessThan2(n)
  }

  def rollDie: Rand[Int] = map(nonNegtiveLessThan2(6))(_ + 1)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  def main(args: Array[String]): Unit = {
    val zero = rollDie(SimpleRNG(5))._1

    println(zero)
  }

}