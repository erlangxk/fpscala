package coder.simon.scalaz.trans

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.~>

object TT extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  trait Monad[M[_]] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def unit[A](a: A): M[A]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma) { a => unit(f(a)) }
  }

  //  object Monad {
  //    def apply[M[_]: Monad]: Monad[M] = implicitly[Monad[M]]
  //  }

  trait Yoneda[F[_], A] { yo =>

    def apply[B](f: A => B): F[B]

    def run: F[A] = apply(a => a)

    def toCoyoneda: Coyoneda[F, A] = new Coyoneda[F, A] {
      type I = A
      def fi = yo.run
      def k(i: A) = i
    }

    def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
      def apply[C](g: B => C): F[C] = yo.apply(f andThen g)
    }
  }

  object Yoneda {
    def apply[F[_]: Functor, A](fa: F[A]) = new Yoneda[F, A] {
      def apply[B](f: A => B): F[B] = Functor[F].map(fa)(f)
    }

    implicit def yonedaFunctor[F[_]] = new Functor[({ type l[x] = Yoneda[F, x] })#l] {
      def map[A, B](ya: Yoneda[F, A])(f: A => B) = ya.map(f)
    }
  }

  trait Coyoneda[F[_], A] { coyo =>
    type I
    def fi: F[I]
    def k(i: I): A

    def run(implicit F: Functor[F]): F[A] = F.map(fi)(k)

    def toYoneda(implicit F: Functor[F]): Yoneda[F, A] = new Yoneda[F, A] {
      def apply[B](f: A => B): F[B] = F.map(fi)(i => f(k(i)))
    }

    def map[B](f: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
      type I = coyo.I
      def fi = coyo.fi
      def k(i: I) = f(coyo.k(i))
    }
  }

  object Coyoneda {
    def apply[F[_], A](fa: F[A]): Coyoneda[F, A] = new Coyoneda[F, A] {
      type I = A
      def fi = fa
      def k(a: A) = a
    }

    implicit def coyonedaFunctor[F[_]] = new Functor[({ type l[x] = Coyoneda[F, x] })#l] {
      def map[A, B](ca: Coyoneda[F, A])(f: A => B) = ca.map(f)
    }
  }

  trait Free[F[_], A] {
    private case class FlatMap[B](a: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    def unit(a: A): Free[F, A] = Return(a)

    def flatMap[B](f: A => Free[F, B])(implicit F: Functor[F]): Free[F, B] = this match {
      case Return(a)     => f(a)
      case Suspend(ffa)  => Suspend(F.map(ffa)(fa => fa.flatMap(f)))
      case FlatMap(b, g) => FlatMap(b, x => g(x).flatMap(f))
    }

    def map[B](f: A => B)(implicit F: Functor[F]): Free[F, B] = flatMap(a => Return(f(a)))

    def resume(implicit F: Functor[F]): Either[F[Free[F, A]], A] = this match {
      case Return(a)  => Right(a)
      case Suspend(k) => Left(k)
      case FlatMap(a, f) => a match {
        case Return(b)     => f(b).resume
        case Suspend(k)    => Left(F.map(k)(x => x.flatMap(f)))
        case FlatMap(b, g) => FlatMap(b, x => g(x).flatMap(f)).resume
      }
    }

    def foldMap[G[_]](f: (F ~> G))(implicit F: Functor[F], G: Monad[G]): G[A] = resume match {
      case Right(a) => G.unit(a)
      case Left(k)  => G.flatMap(f(k))(x => x.foldMap(f))
    }

  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](ffa: F[Free[F, A]]) extends Free[F, A]

  object Free {
    import scalaz.Unapply

    type FreeC[S[_], A] = Free[({ type f[x] = Coyoneda[S, x] })#f, A]

    def liftF[S[_], A](value: => S[A])(implicit F: Functor[S]): Free[S, A] = Suspend(F.map(value)(Return[S, A]))

    def liftFU[MA](value: => MA)(implicit UA: Unapply[Functor, MA]): Free[UA.M, UA.A] = liftF(UA(value))(UA.TC)

    def liftFC[S[_], A](s: S[A]): FreeC[S, A] = liftFU(Coyoneda(s))

    def runFC[S[_], M[_], A](sa: FreeC[S, A])(interp: S ~> M)(implicit M: Monad[M]): M[A] = sa.foldMap[M](new (({ type l[x] = Coyoneda[S, x] })#l ~> M) {
      def apply[A](cy: Coyoneda[S, A]): M[A] = M.map(interp(cy.fi))(cy.k)
    })

  }

  trait Console[A]
  case object GetLine extends Console[String]
  case class PutLine(line: String) extends Console[Unit]

  import Free._

  implicit def liftConsole[A](ca: Console[A]): FreeC[Console, A] = liftFC(ca)

  val xxx = for {
    _ <- PutLine("What is your first name?")
    first <- GetLine
    _ <- PutLine("what is your last name?")
    last <- GetLine
    _ <- PutLine(s"Hello, $first, $last")

  } yield ()

  type Id[A] = A
  implicit val idMonad = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A, B](fa: A)(f: A => B): B = f(fa)
  }

  object RealConsole extends (Console ~> Id) {
    def apply[A](ca: Console[A]): A = ca match {
      case GetLine    => readLine
      case PutLine(l) => println(l)
    }
  }

  case class State[S, A](runState: S => (A, S)) {
    def map[B](f: A => B) = State[S, B](s => {
      val (a1, s1) = runState(s)
      (f(a1), s1)
    })

    def flatMap[B](f: A => State[S, B]) = State[S, B](s => {
      val (a1, s1) = runState(s)
      f(a1).runState(s1)
    })
  }

  case class InOutLog(inLog: List[String], outLog: List[String])
  type LogState[A] = State[InOutLog, A]

  implicit val logStateMonad = new Monad[LogState] {
    def unit[A](a: A) = State(s => (a, s))
    def flatMap[A, B](sa: LogState[A])(f: A => LogState[B]) = sa.flatMap(f)
  }

  object MockConsole extends (Console ~> LogState) {
    def apply[A](c: Console[A]): LogState[A] = State(
      s => (c, s) match {
        case (GetLine, InOutLog(in, out))    => (in.head, InOutLog(in.tail, out))
        case (PutLine(l), InOutLog(in, out)) => ((), InOutLog(in, l :: out))
      })
  }

  val s = Free.runFC(xxx)(MockConsole)
  val ls = s.runState(InOutLog(List("Tiger", "XXXX", "YYYY"), List()))
  println(ls)

}