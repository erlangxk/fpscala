package coder.simon.ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(a) => f(a)
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter(f: A => Boolean): Option[A] = this match {
    case None    => None
    case Some(a) => if (f(a)) this else None
  }

  def filter2(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None    => ob
    case Some(a) => this
  }

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this.map(x => Some(x)).getOrElse(ob)
}

case object None extends Option[Nothing]
case class Some[+A](a: A) extends Option[A]

object Option {
  def mean(xs: Seq[Double]) = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]) = mean(xs) flatMap { m =>
    mean(xs.map(x => scala.math.pow(x - m, 2)))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => None
    case h :: t => h.flatMap(x => sequence(t).map(l => x :: l))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil    => None
    case h :: t => f(h).flatMap(x => traverse(t)(f).map(l => x :: l))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}
