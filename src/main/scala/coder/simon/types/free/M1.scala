package coder.simon.types.free

import scala.language.higherKinds
import scalaz.Functor

object M1 extends App {

  sealed trait FreeMonoid[+A]
  final case object Zero extends FreeMonoid[Nothing]
  final case class Append[A](l: A, r: FreeMonoid[A]) extends FreeMonoid[A]

  def listOp[A](l: List[A]): FreeMonoid[A] = l match {
    case Nil    => Zero
    case h :: t => Append(h, listOp(t))
  }

  println(listOp(List(1, 3, 2, 4)))

  sealed trait Free[F[_], A] {
    def point(a: A) = Return[F, A](a)
    def flatMap[B](f: A => Free[F, B])(implicit functor: Functor[F]): Free[F, B] = this match {
      case Return(a)    => f(a)
      case Suspend(ffa) => Suspend[F, B](functor.map(ffa)(fa => fa flatMap f))
    }

    def map[B](f: A => B)(implicit functor: Functor[F]) = flatMap(x => Return[F, B](f(x)))

    def join(ffa: F[Free[F, A]]): Free[F, A] = Suspend[F, A](ffa)

  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](ffa: F[Free[F, A]]) extends Free[F, A]

}