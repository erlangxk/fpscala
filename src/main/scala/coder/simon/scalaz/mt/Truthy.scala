package coder.simon.scalaz.mt
import scala.language.implicitConversions

trait CanTruthy[A] {
  def truthys(a: A): Boolean
}

object CanTruthy {

  implicit val IntTruthy = new CanTruthy[Int] {
    def truthys(a: Int) = if (a == 0) true else false
  }

  implicit val StringTruthy = new CanTruthy[String] {
    def truthys(s: String) = if (s.length == 0) false else true
  }

  implicit class ToTruthy[A: CanTruthy](v: A) {
    final def truthy: Boolean = implicitly[CanTruthy[A]].truthys(v)
  }
}

object Main {
  import CanTruthy._
  def main(args: Array[String]): Unit = {
    println(0.truthy)
    println(100.truthy)
    
    println("".truthy)
    println("abc".truthy)
  }
}