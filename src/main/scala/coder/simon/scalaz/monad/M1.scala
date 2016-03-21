package coder.simon.scalaz.monad

import scalaz._
import Scalaz._

object M1 {

  sealed trait WsResult[+A]

  final case class WsResultOk[A](trxId: A) extends WsResult[A]
  final case class WsResultErr(errorCode: Int) extends WsResult[Nothing]

  implicit val wsresultmonad = new Monad[WsResult] {

    def bind[A, B](ws: WsResult[A])(f: A => WsResult[B]) = ws match {
      case WsResultOk(trxId)    => f(trxId)
      case err @ WsResultErr(_) => err
    }
    def point[A](a: => A) = WsResultOk(a)

  }

  def main(args: Array[String]): Unit = {
    val w1 = Monad[WsResult].point("123")
    val w2 = Monad[WsResult].point("234")
    for {
      a1 <- w1
      a2 <- w2
    } yield a1 + a2
  }

}