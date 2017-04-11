package coder.simon.scalaz.trans

import scalaz._
import Scalaz._
import scala.language.higherKinds

object T1 extends App {

  def toList[T](opt: Option[T]): List[T] = opt.toList

  val hOptFun = toList _

  val opt2List = new (Option ~> List) {
    def apply[T](opt: Option[T]): List[T] = opt.toList
  }

  println(opt2List(None))
  println(opt2List(Some("hi")))
  println(opt2List(Some(1)))

  sealed trait FreeMonoid[+A]
  final case object Zero extends FreeMonoid[Nothing]
  final case class Append[A](l: A, r: FreeMonoid[A]) extends FreeMonoid[A]

  def listOp[A](l: List[A]): FreeMonoid[A] = l match {
    case Nil    => Zero
    case l :: r => Append(l, listOp(r))
  }

  //println(listOp(List('A,'B,'C)))

  sealed trait Free[F[_], A] {
    def point(a: A) = Return(a)

    def flatMap[B](f: A => Free[F, B])(implicit F: Functor[F]): Free[F, B] = this match {
      case Return(a)    => f(a)
      case Suspend(ffa) => Suspend(F.map(ffa){fa => fa.flatMap(f)})
    }

    def map[B](f: A => B)(implicit F: Functor[F]): Free[F, B] = flatMap { a => Return(f(a)) }

    def join(ffa: F[Free[F, A]]) = Suspend(ffa)
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](ffa: F[Free[F, A]]) extends Free[F, A]

}

object T2 extends App {

  trait Config[+A] {
    def get: A
  }

  object Config {
    def apply[A](a: A): Config[A] = new Config[A] { def get = a }
    implicit val configFunctor = new Functor[Config] {
      def map[A, B](ca: Config[A])(f: A => B): Config[B] = new Config[B] { def get = f(ca.get) }
    }
  }

  val fc = Free.liftF(Config("aaaa"))

  trait Interact[+A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  val x = Free.liftFC(Ask("how are you"))
  val y = Free.liftFC(Tell("cool"))
  

}