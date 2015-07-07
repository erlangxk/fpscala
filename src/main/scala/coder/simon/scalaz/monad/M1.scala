package coder.simon.scalaz.monad

import scalaz._
import Scalaz._

object M1 {

  sealed trait WsResult[+A]

  case class WsResultOk[A](trxId: A) extends WsResult[A]
  case class WsResultErr(errorCode: Int) extends WsResult[Nothing]

  implicit val wsresultmonad = new Monad[WsResult] {

    def bind[A, B](ws: WsResult[A])(f: A => WsResult[B]) = ws match {
      case WsResultOk(trxId)    => f(trxId)
      case err @ WsResultErr(_) => err
    }
    def point[A](a: => A) = WsResultOk(a)

  }

  def main(args: Array[String]) = {
    val w1 = WsResultOk("123")
    val w2 = WsResultOk("234")
    //TODO how to turn own class as monad
  }

}