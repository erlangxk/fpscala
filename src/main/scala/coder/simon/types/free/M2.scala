package coder.simon.types.free

object M2 extends App {
  trait KeyLog[A] { self =>
    def value: A
    def log: String
    override def toString = s"[${value},${log}]"

    def flatMap[B](f: A => KeyLog[B]) = new KeyLog[B] {
      val k = f(self.value)
      val value = k.value
      val log = s"${self.log} - ${k.log}"
    }
  }

  object KeyLog {
    def apply[A](key: A, msg: String) = new KeyLog[A] {
      val value = key
      val log = msg
    }

  }

  val k = KeyLog(3, "enter number 3").flatMap(x => KeyLog(x + 4, "add 4"))
  println(k)

  import scalaz._
  import Scalaz._

  implicit object keyLogMonad extends Monad[KeyLog] {
    def point[A](a: => A) = KeyLog(a, "")
    def bind[A, B](kl: KeyLog[A])(f: A => KeyLog[B]): KeyLog[B] = kl flatMap f
  }

  val y = Monad[KeyLog].point(3) >>= { z => KeyLog(z, s"I am $z") } >>= { x => KeyLog(x + 4, "add 4") } >>= { y => KeyLog(y - 100, "minus 100") }
  println(y)

  def change(desc: String)(v: Int) = KeyLog(v, desc)

  val z = for {
    k1 <- change("set 3")(3)
    k2 <- change("add 4")(k1 + 4)
    k3 <- change("minus 100")(k2 - 100)
  } yield k3
  
  val xxx= 1 |-> 50

  println(z)
  println(xxx)
  
  println(Enum[Int].min)
}