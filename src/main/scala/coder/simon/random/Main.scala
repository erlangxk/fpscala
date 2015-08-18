package coder.simon.random

import org.uncommons.maths.random.DevRandomSeedGenerator
import org.uncommons.maths.random.SeedGenerator
import org.uncommons.maths.random.AESCounterRNG


object Main {

  def run[A](name: String)(f: => A): (A, Long) = {
    val t = new java.util.Date().getTime
    val v = f
    val t2 = new java.util.Date().getTime
    println(s"$name,${t2 - t}")
    (v, t2)
  }

  def main(args: Array[String]): Unit = {

    val (bytes2, t4) = run("uncommon maths") {
      val s: SeedGenerator = new DevRandomSeedGenerator
      s.generateSeed(16)
    }
    println(bytes2.length)
    
    val (bytes, t2) = run("java.security.securerandom")(
      java.security.SecureRandom.getSeed(16))
    println(bytes.length)

    val random = new java.security.SecureRandom(bytes)
    for (_ <- 1 to 100000) {
      println(random.nextInt())
    }
    val t3 = new java.util.Date().getTime
    println(s"${t3 - t2}")

    
    val random2 = new AESCounterRNG(bytes2)
    for (_ <- 1 to 100000) {
      println(random2.nextInt())
    }
    val t5 = new java.util.Date().getTime
    println(s"${t5 - t4}")

  }
}