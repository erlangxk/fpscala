package coder.simon.scalaz

import scalaz._
import Scalaz._
import scalaz.OptionT._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

object FutureO {

  def main(args: Array[String]) = {
    val oa = "a".some
    val ob = "b".some

    val oab = for {
      a <- oa
      b <- ob
    } yield a + b

    println(oab)

    def fa = Future.successful("a111")
    def fb(a: String) = Future.successful(a + "b222")

    val fab = for {
      a <- fa
      ab <- fb(a)
    } yield ab

    val ab = Await.result(fab, Duration(3, TimeUnit.SECONDS))
    println(ab)

    def foa: Future[Option[String]] = Future.successful(Some("1111"))
    def fob(a: String): Future[Option[String]] = Future.successful(Some(a + "b222"))

    //    couldn't compose any more
    //    val cfab= for {
    //      oa <- foa
    //      a <- oa
    //      ab <- fob(a)
    //    } yield ab

    val cfab = for {
      a <- optionT(foa)
      ab <- optionT(fob(a))
    } yield ab

    val cab = Await.result(cfab.run, Duration(3, TimeUnit.SECONDS))
    println(cab)
  }

}