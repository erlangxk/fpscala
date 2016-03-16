package coder.simon.types.adhocpolymophism

import scala.language.implicitConversions

object M1 extends App {

  trait Plus[A] {
    def add(a1: A, a2: A): A
  }

  def plus[A: Plus](a1: A, a2: A): A = {
    val plus = implicitly[Plus[A]]
    plus.add(a1, a2)
  }

  trait PlusOps[A] {
    def add(a: A): A
  }

  implicit def toPlus[A: Plus](a: A): PlusOps[A] = new PlusOps[A] {
    def add(b: A): A = plus(a, b)
  }

  implicit def plusString: Plus[String] = new Plus[String] {
    def add(a1: String, a2: String) = a1 + a2
  }

  implicit def plusInt: Plus[Int] = new Plus[Int] {
    def add(a1: Int, a2: Int) = a1 + a2
  }

  println("aaa".add("bbb"))
  println(plus("aaa", "bbb"))

  println(3.toString.add("4"))
}