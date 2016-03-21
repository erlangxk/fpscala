package coder.simon.scalaz.monad

import scalaz._
import Scalaz._

import scala.language.implicitConversions

object M2 extends App {

  sealed trait TrafficLight extends Product with Serializable

  final case object Red extends TrafficLight
  final case object Amber extends TrafficLight
  final case object Green extends TrafficLight

  implicit object equalTrafficLight extends Equal[TrafficLight] {
    override def equal(a: TrafficLight, b: TrafficLight) = (a, b) match {
      case (Red, Red)     => true
      case (Amber, Amber) => true
      case (Green, Green) => true
      case _              => false
    }
  }

  println(Equal[TrafficLight].equal(Red, Amber))
  println(1 === 1)

  trait Truthy[A] {
    def isTrue(a: A): Boolean
  }

  object Truthy {
    def apply[A](f: A => Boolean) = new Truthy[A] {
      override def isTrue(a: A) = f(a)
    }
  }

  trait TruthyOps[A] {
    def value: A
    def F: Truthy[A]
    def isTrue: Boolean = F.isTrue(value)
  }

  implicit def toTruthyOps[A](a:A)(implicit truthy:Truthy[A]) = new TruthyOps[A] {
    val value=a
    val F=truthy
  }

  implicit def int2Truthy = Truthy[Int](v => if (v > 0) true else false)
  3.isTrue
 
}