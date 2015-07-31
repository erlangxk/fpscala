package coder.simon.scalaz.mt

import scalaz._
import Scalaz._

object MT3 {

  def myName(step: String): Reader[String, String] = Reader { x => step + ", I am " + x }

  def localExample: Reader[String, (String, String, String)] = for {
    a <- myName("First")
    b <- myName("Second") >=> Reader { _ + "dy" }
    c <- myName("Third")
  } yield (a, b, c)

  type ReaderTOption[A, B] = ReaderT[Option, A, B]
  object ReaderTOption {
    def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
  }

  def configure(key: String) = ReaderTOption((x: Map[String, String]) => x.get(key))

  def setupConnection = for {
    host <- configure("host")
    user <- configure("user")
    password <- configure("password")
  } yield (host, user, password)

  type ReaderToX[C] = ReaderTOption[C, _]
  type StateTReaderTOption[C, S, A] = StateT[({ type l[X] = ReaderTOption[C, X] })#l, S, A]

  object StateTReaderTOption extends StateTInstances with StateTFunctions {
    def apply[C, S, A](f: S => (S, A)) = new StateTReaderTOption[C, S, A] {
      def apply(s: S) = f(s).point[({ type l[X] = ReaderTOption[C, X] })#l]
    }

    def get[C, S]: StateTReaderTOption[C, S, S] = StateTReaderTOption { s => (s, s) }

    def put[C, S](s: S): StateTReaderTOption[C, S, Unit] = StateTReaderTOption { s => (s, ()) }
  }

  type Stack = List[Int]
  type Config = Map[String, String]
  def configure2[S](key: String) = new StateTReaderTOption[Config, S, String] {
    def apply(s: S) = ReaderTOption[Config, (S, String)] { config: Config => config.get(key) map { (s, _) } }
  }

  def push(x: Int): StateTReaderTOption[Config, Stack, Unit] = {
    import StateTReaderTOption.{ get, put }
    for {
      xs <- get[Config, Stack]
      r <- put(x :: xs)
    } yield r
  }

  def stackManip: StateTReaderTOption[Config, Stack, Unit] = for {
    x <- configure2("x")
    a <- push(x.toInt)
  } yield a

  def main(args: Array[String]): Unit = {

    val s = stackManip(List(5, 8, 2, 1))

    println(s(Map("x" -> "7")))
    println(s(Map("y" -> "1")))

    val r = localExample("Fred")
    println(r)

    val goodCfg = Map("host" -> "hosta", "user" -> "dummy", "password" -> "*****")

    val r2 = setupConnection(goodCfg)
    println(r2)

    val badCfg = Map("host" -> "hostb")
    val r3 = setupConnection(badCfg)
    println(r3)
  }

}