package coder.simon.ch5

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def headOption2: Option[A] = foldRight(None: Option[A]) {
    (e, acc) => Some(e)
  }

  def startsWith[A](s: Stream[A]): Boolean = this.zipWith(s).forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty          => None
    case s @ Cons(_, t) => Some((s, t()))
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) {
    (e, acc) => cons(f(e), acc)
  }

  def mapX[B](f: A => B): Stream[B] = unfold(this) {
    case Empty      => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (e, acc) => if (!f(e)) acc else cons(e, acc)
  }

  def append[B >: A](sb: => Stream[B]): Stream[B] = foldRight(sb) {
    (e, acc) => cons(e, acc)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) {
    (e, acc) => f(e).append(acc)
  }

  def toList: List[A] = {
    def loop(s: Stream[A], temp: List[A]): List[A] = s match {
      case Empty      => temp
      case Cons(h, t) => loop(t(), h() :: temp)
    }
    loop(this, Nil: List[A]).reverse
  }

  def take(n: Int): Stream[A] = (this, n) match {
    case (Empty, _)      => empty
    case (_, 0)          => empty
    case (Cons(h, t), _) => cons(h(), t().take(n - 1))
  }

  def takeX(n: Int): Stream[A] = unfold(this) {
    case Cons(h, t) if n > 0 => Some((h(), t()))
    case _                   => None
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _                      => empty
  }

  def takeWhileX(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (p(h())) => Some((h(), t()))
    case _                      => None
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => cons(z, empty[B])
    case Cons(h, t) =>
      val r = t().scanRight(z)(f)
      val Cons(h2, _) = r
      val x = f(h(), h2())
      cons(x, r)
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((e, acc) => p(e) && acc)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (e, acc) => if (p(e)) cons(e, acc) else acc
  }

  def drop(n: Int): Stream[A] = (this, n) match {
    case (Empty, _)      => empty
    case (_, 0)          => this
    case (Cons(_, t), _) => t().drop(n - 1)
  }

  def zipWith[B](sb: Stream[B]): Stream[(A, B)] = (this, sb) match {
    case (Cons(ha, ta), Cons(hb, tb)) => cons((ha(), hb()), ta().zipWith(tb()))
    case _                            => empty
  }

  def zipWithX[B](sb: Stream[B]): Stream[(A, B)] = unfold((this, sb)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
    case _                            => None
  }

  def zipAll[B](sb: Stream[B]): Stream[(Option[A], Option[B])] = (this, sb) match {
    case (Cons(ha, ta), Cons(hb, tb)) => cons((Some(ha()), Some(hb())), ta().zipAll(tb()))
    case (Cons(ha, ta), Empty)        => cons((Some(ha()), None), ta().zipAll(Empty))
    case (Empty, Cons(hb, tb))        => cons((None, Some(hb())), Empty.zipAll(tb()))
    case (Empty, Empty)               => empty[(Option[A], Option[B])]
  }

  def zipAllX[B](sb: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, sb)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (Cons(ha, ta), Empty)        => Some((Some(ha()), None), (ta(), Empty))
    case (Empty, Cons(hb, tb))        => Some((None, Some(hb())), (Empty, tb()))
    case (Empty, Empty)               => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def ones2 = constant(1)

  def fibs = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a + b))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => empty
    case Some((v, s)) => cons(v, unfold(s)(f))
  }

  def fibs2 = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }
  def from2(n: Int) = unfold(n) { x => Some((x, x + 1)) }
  def constant2[A](a: A): Stream[A] = unfold(a) { x => Some((x, x)) }
  def ones3: Stream[Int] = unfold(1) { _ => Some((1, 1)) }

}

object Main {
  def main(args: Array[String]) {
    val s = Stream(1, 3, 4, 5, 7)

    println(s)
    println(s.headOption)
    println(s.headOption2)

    val s2 = Stream.empty[Int]
    println(s2.headOption)
    println(s2.headOption2)

    val l = s.toList
    println(l)

    val l2 = s.take(3)
    println(l2)
    println(l2.toList)

    val l3 = s.drop(3)
    println(l3)
    println(l3.toList)

    val l4 = s.takeWhile { x => x < 6 }
    val l42 = s.takeWhile2 { x => x < 6 }
    println(l4)
    println(l4.toList)
    println(l42)
    println(l42.toList)

    println("XXXXXXXXXXXXX")

    val x = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).map(_ + 10).filter(_ % 2 == 0)

    println("YYYYYYYYYY")
    x.toList

    println(Stream.ones.take(5).toList)
    println(Stream.ones2.take(5).toList)
    println(Stream.ones3.take(5).toList)
    println(Stream.constant(2).take(5).toList)
    println(Stream.constant2(2).take(5).toList)

    println(Stream.from(100).take(5).toList)
    println(Stream.from2(100).take(5).toList)

    println(Stream.fibs.take(7).toList)
    println(Stream.fibs2.take(7).toList)

    val ul = Stream.unfold(1)(s => if (s > 7) None else Some((s"${s + 7}kao"), s + 1))
    println(ul.toList)

    val sb = Stream("a", "b", "c", "d", "e", "f", "g")

    val sasb = s.zipWith(sb)
    println(sasb.toList)
    val sasbx = s.zipWithX(sb)
    println(sasbx.toList)

    val sasball = s.zipAll(sb)
    println(sasball.toList)
    val sasballx = s.zipAllX(sb)
    println(sasballx.toList)

    val ts = s.tails
    println(ts.toList.map(_.toList))

    val bbb1 = s.hasSubsequence(Stream(4, 5))
    println(bbb1)
    val bbb2 = s.hasSubsequence(Stream(5, 4))
    println(bbb2)

    val sr = Stream(1, 2, 3).scanRight(0)(_ + _).toList
    println(sr)

  }

}