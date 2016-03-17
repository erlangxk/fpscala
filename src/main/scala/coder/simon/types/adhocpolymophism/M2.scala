package coder.simon.types.adhocpolymophism

import scala.language.higherKinds
import scala.language.implicitConversions 

object M2 extends App {

  trait Monoid[A] {
    def mappend(a: A, b: A): A
    def mzero: A
  }

  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A
    def |+|(a2: A) = F.mappend(value, a2)
  }
  
  implicit def toMonoidOp[A:Monoid](a:A):MonoidOp[A]=new MonoidOp[A]{
    val F=implicitly[Monoid[A]]
    val value=a
  }

  implicit object intMonoid extends Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero = 0
  }

  implicit object strMonoid extends Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero = ""
  }
  
  println(3 |+| 4)

  println(sum(List(1, 2, 3, 4, 5)))
  println(sum(List(1, 2, 3, 4, 5).map(_.toString)))

  trait FoldLeft[M[_]] {
    def foldLeft[A, B](c: M[A], acc: B, f: (B, A) => B): B
  }

  implicit object listFoldLeft extends FoldLeft[List] {
    def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B) = l.foldLeft(acc)(f)
  }

  def sum[A: Monoid, M[_]: FoldLeft](c: M[A]) = {
    val fl = implicitly[FoldLeft[M]]
    val m = implicitly[Monoid[A]]
    fl.foldLeft(c, m.mzero, m.mappend)
  }

  println(sum(List(1, 3, 3, 4)))

}