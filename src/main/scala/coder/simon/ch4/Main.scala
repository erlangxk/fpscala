package coder.simon.ch4

object Main {

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

}