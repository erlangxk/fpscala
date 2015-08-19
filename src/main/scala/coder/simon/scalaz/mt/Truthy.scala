package coder.simon.scalaz.mt
import scala.language.implicitConversions

trait TruthyOps[A] {
  def value: A
  def f: CanTruthy[A]
  final def truthy: Boolean = f.truthys(value)
}

trait CanTruthy[A] {
  def truthys(a: A): Boolean
}

object CanTruthy {

  def apply[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    override def truthys(a: A) = f(a)
  }

  implicit val IntTruthy = apply[Int]((a) => if (a == 0) false else true)

  implicit def toTruthy[A](v: A)(implicit ev: CanTruthy[A]) = new TruthyOps[A] {
    val value = v
    val f = ev
  }
}

object Main {
  import CanTruthy._
  def main(args: Array[String]): Unit = {
    println(0.truthy)
    println(100.truthy)
  }
}