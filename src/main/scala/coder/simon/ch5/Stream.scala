package coder.simon.ch5

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    def loop(s: Stream[A], temp: List[A]): List[A] = s match {
      case Empty      => temp
      case Cons(h, t) => loop(t(), h() :: temp)
    }
    loop(this, Nil: List[A]).reverse
  }

  def take(n: Int): Stream[A] = {
    println("in take")
    (this, n) match {
      case (Empty, _)      => empty
      case (_, 0)          => empty
      case (Cons(h, t), _) => cons(h(), t().take(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = {
    println("in drop")
    (this, n) match {
      case (Empty, _)      => empty
      case (_, 0)          => this
      case (Cons(_, t), _) => t().drop(n - 1)
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Main {
  def main(args: Array[String]) {
    val s = Stream(1, 3, 4, 5, 7)

    println(s)

    val l = s.toList
    println(l)

    val l2 = s.take(3)
    println(l2)

    val l3 = s.drop(3)
    println(l3)
  }

}