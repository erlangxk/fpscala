package coder.simon.scalaz.kleisli

import scalaz._
import Scalaz._

object K2 extends App {

  val f = (_: Int) + 3
  val g = (_: Int) * 5

  val h1 = f map g
  val h2 = g map f

  println(h1(3))
  println(h2(3))

  val k = (f |@| g)(_ + _)

  println(k(3))

  val n = for {
    a <- f
    b <- g
  } yield a + b
  
  println(n(3))
}