package coder.simon.types.free

import scalaz._
import Scalaz._

object M3 extends App {
  case class Logger[LOG, A](log: LOG, value: A) {
    def map[B](f: A => B): Logger[LOG, B] = Logger(log, f(value))

    def flatMap[B](f: A => Logger[LOG, B])(implicit m: Monoid[LOG]) = {
      val n = f(value)
      Logger(log |+| n.log, n.value)
    }
  }
  object Logger {
    implicit def toLogger[LOG](implicit m: Monoid[LOG]) = new Monad[({ type L[X] = Logger[LOG, X] })#L] {
      def point[A](a: => A) = Logger(m.zero, a)
      def bind[A, B](la: Logger[LOG, A])(f: A => Logger[LOG, B]) = la.flatMap(f)
    }
  }

  final implicit class LoggerOps[A](a: A) {
    def addLog[LOG](log: LOG): Logger[LOG, A] = Logger(log, a)
  }

  //  def enterInt(x: Int) = Logger(s"Enter Int:$x", x)
  //  def enterStr(str: String) = Logger(s"Enter String:$str", str)
  //
  //  val r = for {
  //    a <- enterInt(3)
  //    b <- enterStr("4")
  //  } yield a + b
  //
  //  println(r)

  val r = for {
    a <- 3.addLog("Enter Int 3,")
    b <- "4".addLog("Enter String 4")
  } yield a + b
  println(r)

  val x = 3 set Vector("Enter Int 3")

  val y = "wokao".tell

  def gcd(a: Int, b: Int): Writer[Vector[String], Int] = b match {
    case 0 =>
      Vector(s"Finish at $a").tell map { _ => a }
    case _ =>
      Vector(s"$a mod $b = ${a % b}").tell >>= { _ => gcd(b, a % b) }

  }

  println(gcd(8, 3))

}