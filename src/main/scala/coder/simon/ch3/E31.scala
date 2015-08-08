package coder.simon.ch3

object E31 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil        => 0
      case Cons(h, t) => h + sum(t)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(h, t)   => h * product(t)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](ls: List[A]): List[A] = ls match {
      case Nil        => throw new IllegalArgumentException
      case Cons(_, t) => t
    }

    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
      case (Nil, _)        => Nil
      case (_, 0)          => l
      case (Cons(_, t), _) => drop(t, n - 1)
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil        => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }

    def setHead[A](a: A, l: List[A]): List[A] = l match {
      case Nil        => Cons(a, Nil)
      case Cons(_, t) => Cons(a, t)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil          => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil        => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, z)((acc, e) => f(e, acc))

    def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((e, acc) => f(acc, e))

    def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)
    def sum3(l: List[Int]): Int = foldRight2(l, 0)(_ + _)
    def product2(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)
    def product3(l: List[Double]): Double = foldRight2(l, 1.0)(_ * _)

    def length[A](l: List[A]): Int = foldRight(l, 0)((e, l) => l + 1)

    def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((l, e) => Cons(e, l))

    def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((e, acc) => Cons(f(e), acc))
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concatenate(map(l)(f))

    def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Nil        => Nil
      case Cons(h, t) => append(f(h), flatMap2(t)(f))
    }

    def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((e, acc) => if (f(e)) Cons(e, acc) else acc)
    def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(e => if (f(e)) Cons(e, Nil) else Nil)

    def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

    def concatenate[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])(append)

    def zipSum(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipSum(t1, t2))
      case _                            => Nil
    }

    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _                            => Nil
    }

    def hasSequence[A](l1: List[A], l2: List[A]): Boolean = {
      def isPrefix[A](a1: List[A], a2: List[A]): Boolean = (a1, a2) match {
        case (Nil, Nil)                   => true
        case (Nil, _)                     => false
        case (_, Nil)                     => true
        case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) isPrefix(t1, t2) else false
      }

      def tails[A](l: List[A]): List[List[A]] = l match {
        case Nil        => List(Nil)
        case Cons(h, t) => Cons(l, tails(t))
      }

      def any[A](l: List[A])(f: A => Boolean): Boolean = l match {
        case Nil        => false
        case Cons(h, t) => if (f(h)) true else any(t)(f)
      }

      any(tails(l1))(isPrefix(_, l2))
    }

    val l = List(1, 2, 3, 4, 5)

    val x = l match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }

  }
  import List._
  def main(args: Array[String]): Unit = {
    println(x)
    println(sum(l))
    println(sum2(l))
    println(sum3(l))

    println(reverse(l))

    println(append(l, List(8, 88)))

    println(concatenate(List(l, l, l)))

    println(zipSum(List(2, 3, 4), List(3, 5, 7, 8)))

    println(zipWith(List(2, 3, 4), List(3, 5, 7, 8))((a, b) => (a + b).toString + "aaa"))

    println(product(map(l)(_.toDouble)))
    println(product2(map(l)(_.toDouble)))
    println(product3(map(l)(_.toDouble)))

    println(hasSequence(l, List(1, 2, 3)))
    println(hasSequence(l, List(2, 3, 4)))
    println(hasSequence(l, List(5)))
    println(hasSequence(l, List(1, 2, 2)))

    println(tail(l))
    println(init(l))
    println(length(l))
    println(drop(l, 13))
    println(dropWhile(l, (x: Int) => x < 5))
    println(setHead(7, l))
    // println(tail(Nil))

    println(filter(l)(_ != 3))
    println(flatMap(l)(e => List(e, e + 3)))
  }

}